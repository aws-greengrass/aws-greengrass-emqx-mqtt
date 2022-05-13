/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <aws/greengrass/GreengrassCoreIpcClient.h>
#include <filesystem>
#include <fstream>

#include "logger.h"
#include "private/certificate_updater.h"

#define SUBSCRIBE_TIMEOUT_SECONDS 10

// TODO: naming
static const std::filesystem::path EMQX_KEY_PATH = std::filesystem::path{"key.pem"};
static const std::filesystem::path EMQX_PEM_PATH = std::filesystem::path{"cert.pem"};
static const std::filesystem::path EMQX_CA_PATH = std::filesystem::path{"cacert.pem"};

int CertificateUpdatesHandler::writeCertsToFiles(
    Aws::Crt::String &privateKeyValue, Aws::Crt::String &certValue,
    std::vector<Aws::Crt::String, Aws::Crt::StlAllocator<Aws::Crt::String>> &allCAsValue) {
    // TODO improve io error handling

    if (!basePath) {
        return -1;
    }
    const auto path = *basePath;
    std::filesystem::create_directories(path);
    std::ofstream out_key(path / EMQX_KEY_PATH);
    out_key << privateKeyValue.c_str();
    out_key.close();

    std::ofstream out_pem(path / EMQX_PEM_PATH);
    out_pem << certValue.c_str();
    out_pem.close();

    std::ofstream out_ca(path / EMQX_CA_PATH);
    // TODO: ensure this chain order matches https://www.rfc-editor.org/rfc/rfc4346#section-7.4.2
    for (const auto &str : allCAsValue) {
        out_ca << str << std::endl;
    }
    out_ca.close();

    return 0;
}

void CertificateUpdatesHandler::OnStreamEvent(GG::CertificateUpdateEvent *response) {
    // TODO: Improve this code with error handling, logging

    LOG_I(CERT_UPDATER_SUBJECT, "Retrieving all certs...");
    auto certUpdate = response->GetCertificateUpdate();
    if (!certUpdate) {
        LOG_E(CERT_UPDATER_SUBJECT, "Failed to get certificate update");
        return;
    }

    auto privateKey = certUpdate->GetPrivateKey();
    if (!privateKey || privateKey->empty()) {
        LOG_E(CERT_UPDATER_SUBJECT, "Failed to get private key");
        return;
    }

    auto cert = certUpdate->GetCertificate();
    if (!cert || cert->empty()) {
        LOG_E(CERT_UPDATER_SUBJECT, "Failed to get cert");
        return;
    }

    auto allCAs = certUpdate->GetCaCertificates();
    if (!allCAs) {
        LOG_E(CERT_UPDATER_SUBJECT, "Failed to get CA certs");
        return;
    }

    int writeStatus = writeCertsToFiles(privateKey.value(), cert.value(), allCAs.value());
    if (writeStatus != 0) {
        LOG_E(CERT_UPDATER_SUBJECT, "Failed to write certificates to files with code %d", writeStatus);
    } else {
        LOG_I(CERT_UPDATER_SUBJECT, "Updated all certs!");
    }

    if (subscription) {
        subscription->operator()(response);
    }
}

bool CertificateUpdatesHandler::OnStreamError(OperationError *error) {
    LOG_E(CERT_UPDATER_SUBJECT, "OnStream error %s", error->GetMessage().value().c_str());
    return false; // Return true to close stream, false to keep stream open.
}

void CertificateUpdatesHandler::OnStreamClosed() {
    LOG_I(CERT_UPDATER_SUBJECT, "Stream closed");
    // Handle close.
}

int CertificateUpdater::subscribeToUpdates(
    std::unique_ptr<std::filesystem::path> basePath,
    std::unique_ptr<std::function<void(GG::CertificateUpdateEvent *)>> subscription) {
    GG::SubscribeToCertificateUpdatesRequest request;
    GG::CertificateOptions options;
    options.SetCertificateType(GG::CERTIFICATE_TYPE_SERVER);
    request.SetCertificateOptions(options);

    updatesHandler = std::make_shared<CertificateUpdatesHandler>(std::move(basePath), std::move(subscription));
    operation = ipcClient.NewSubscribeToCertificateUpdates(updatesHandler);
    if (!operation) {
        LOG_E(CERT_UPDATER_SUBJECT, "Failed creating SubscribeToCertificateUpdatesOperation.");
        // TODO: Improve return codes
        return -1;
    }

    auto activate = operation->Activate(request, nullptr);
    activate.wait();

    auto responseFuture = operation->GetResult();
    if (responseFuture.wait_for(std::chrono::seconds(SUBSCRIBE_TIMEOUT_SECONDS)) == std::future_status::timeout) {
        LOG_E(CERT_UPDATER_SUBJECT, "Operation timed out while waiting for response from Greengrass Core.");
        // TODO: Improve return codes
        return -2;
    }

    // TODO: Improve error handling
    auto response = responseFuture.get();
    if (!response) {
        // Handle error.
        auto responseType = response.GetResultType();
        LOG_E(CERT_UPDATER_SUBJECT, "Subscribe failed with response type %d", responseType);
        if (responseType == OPERATION_ERROR) {
            auto *error = response.GetOperationError();
            if (error != nullptr) {
                LOG_E(CERT_UPDATER_SUBJECT, "Cert updates subscribe failure response: %s",
                      error->GetMessage().value().c_str());
            }
        } else {
            // Handle RPC error.
        }
        // TODO: Improve return codes
        return -1;
    }
    LOG_I(CERT_UPDATER_SUBJECT, "Successfully subscribed to cert updates");
    return 0;
}
