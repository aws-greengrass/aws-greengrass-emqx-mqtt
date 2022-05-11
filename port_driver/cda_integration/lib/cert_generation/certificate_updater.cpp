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

static const std::filesystem::path EMQX_KEY_PATH = std::filesystem::path{"etc/greengrass_certs/greengrass_emqx.key"};
static const std::filesystem::path EMQX_PEM_PATH = std::filesystem::path{"etc/greengrass_certs/greengrass_emqx.pem"};
static const std::filesystem::path EMQX_CA_PATH = std::filesystem::path{"etc/greengrass_certs/greengrass_ca.pem"};

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

    auto allCas = certUpdate->GetCaCertificates();
    if (!allCas) {
        LOG_E(CERT_UPDATER_SUBJECT, "Failed to get CA certs");
        return;
    }

    // TODO: Fix this code to write the cert and key files correctly
    auto cwd = std::filesystem::current_path();
    std::ofstream out_key(cwd / EMQX_KEY_PATH);
    out_key << privateKey.value().c_str();
    out_key.close();
    std::ofstream out_pem(cwd / EMQX_PEM_PATH);
    out_pem << cert.value().c_str();
    out_pem.close();
    std::ofstream out_ca(cwd / EMQX_CA_PATH);
    out_ca << allCas.value().front().c_str();
    out_ca.close();
    LOG_I(CERT_UPDATER_SUBJECT, "Updated all certs!");
}

bool CertificateUpdatesHandler::OnStreamError(OperationError *error) {
    LOG_E(CERT_UPDATER_SUBJECT, "OnStream error %s", error->GetMessage().value().c_str());
    return false; // Return true to close stream, false to keep stream open.
}

void CertificateUpdatesHandler::OnStreamClosed() {
    LOG_I(CERT_UPDATER_SUBJECT, "Stream closed");
    // Handle close.
}

int CertificateUpdater::subscribeToUpdates() {
    GG::SubscribeToCertificateUpdatesRequest request;
    GG::CertificateOptions options;
    options.SetCertificateType(GG::CERTIFICATE_TYPE_SERVER);
    request.SetCertificateOptions(options);

    auto operation = ipcClient.NewSubscribeToCertificateUpdates(updatesHandler);
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
