/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include "private/certificate_updater.h"
#include "logger.h"
#include <aws/greengrass/GreengrassCoreIpcClient.h>
#include <filesystem>
#include <fstream>

#define SUBSCRIBE_TIMEOUT_SECONDS 10

// TODO: naming
static const std::filesystem::path EMQX_KEY_PATH = std::filesystem::path{"key.pem"};
static const std::filesystem::path EMQX_PEM_PATH = std::filesystem::path{"cert.pem"};

CertWriteStatus CertificateUpdatesHandler::writeCertsToFiles(const Aws::Crt::String &privateKeyValue,
                                                             const Aws::Crt::String &certValue,
                                                             const Aws::Crt::Vector<Aws::Crt::String> &caCerts) {

    if (!basePath) {
        return CertWriteStatus::WRITE_ERROR_BASE_PATH;
    }

    const auto path = *basePath;
    try {
        if (std::filesystem::create_directories(path)) {
            LOG_D(CERT_UPDATER_SUBJECT, "Created directory %s to write certs to", path.c_str());
        }
    } catch (std::exception &e) {
        LOG_E(CERT_UPDATER_SUBJECT, "Failed to create write directory %s for certs: %s", path.c_str(), e.what());
        return CertWriteStatus::WRITE_ERROR_DIR_PATH;
    }

    try {
        auto out_key_path = std::ofstream(path / EMQX_KEY_PATH);
        out_key_path << privateKeyValue.c_str();
        out_key_path.close();

        // write the entire cert chain as a pem file
        auto out_pem_path = std::ofstream(path / EMQX_PEM_PATH);
        out_pem_path << certValue.c_str();
        for (const auto &caCert : caCerts) {
            out_pem_path << std::endl << caCert.c_str();
        }
        out_pem_path.close();
    } catch (std::exception &e) {
        // TODO: unit test for this branch
        LOG_E(CERT_UPDATER_SUBJECT, "Could not write certs to directory: %s", e.what());
        return CertWriteStatus::WRITE_ERROR_OUT_STREAM;
    }

    return CertWriteStatus::WRITE_SUCCESS;
}

void CertificateUpdatesHandler::OnStreamEvent(GG::CertificateUpdateEvent *response) {

    LOG_I(CERT_UPDATER_SUBJECT, "Retrieving all certs...");
    if (response == nullptr) {
        LOG_E(CERT_UPDATER_SUBJECT, "Received CertificateUpdateEvent response was null");
        return;
    }

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

    const auto allCAs = certUpdate->GetCaCertificates();
    if (!allCAs || allCAs->empty()) {
        LOG_E(CERT_UPDATER_SUBJECT, "Failed to get CA certs");
        return;
    }

    const CertWriteStatus writeStatus = writeCertsToFiles(privateKey.value(), cert.value(), allCAs.value());
    if (writeStatus != CertWriteStatus::WRITE_SUCCESS) {
        LOG_E(CERT_UPDATER_SUBJECT, "Failed to write certificates to files with code %d", (int)writeStatus);
        return;
    }

    LOG_I(CERT_UPDATER_SUBJECT, "Updated all certs!");
    if (subscription) {
        subscription->operator()(response);
    }
}

bool CertificateUpdatesHandler::OnStreamError(OperationError *error) {
    if (error != nullptr) {
        LOG_E(CERT_UPDATER_SUBJECT, "OnStream error %s", error->GetMessage().value().c_str());
    }
    return false; // Return true to close stream, false to keep stream open.
}

void CertificateUpdatesHandler::OnStreamClosed() { LOG_I(CERT_UPDATER_SUBJECT, "Stream closed"); }

CertSubscribeUpdateStatus CertificateUpdater::subscribeToUpdates(
    std::unique_ptr<std::filesystem::path> basePath,
    std::unique_ptr<std::function<void(GG::CertificateUpdateEvent *)>> subscription) {

    if (subscribed) {
        subscriptionActive->exchange(true);
        LOG_I(CERT_UPDATER_SUBJECT, "Subscription already exists, ignoring subscription request");
        return CertSubscribeUpdateStatus::SUBSCRIBE_SUCCESS;
    }

    GG::SubscribeToCertificateUpdatesRequest request;
    GG::CertificateOptions options;
    options.SetCertificateType(GG::CERTIFICATE_TYPE_SERVER);
    request.SetCertificateOptions(options);

    std::shared_ptr<std::function<void(GG::CertificateUpdateEvent *)>> subscriptionCapture = std::move(subscription);
    auto updatesHandlerWrapper = std::make_unique<std::function<void(Aws::Greengrass::CertificateUpdateEvent *)>>(
        [subscriptionCapture = subscriptionCapture,
         subscriptionActive = subscriptionActive]([[maybe_unused]] Aws::Greengrass::CertificateUpdateEvent *event) {
            if (!subscriptionCapture) {
                return;
            }
            if (!subscriptionActive->load()) {
                LOG_I(CERT_UPDATER_SUBJECT, "Ignoring received certificate update, subscription is not active");
                return;
            }
            subscriptionCapture->operator()(event);
        });

    updatesHandler = std::make_shared<CertificateUpdatesHandler>(std::move(basePath), std::move(updatesHandlerWrapper));
    operation = ipcClient.NewSubscribeToCertificateUpdates(updatesHandler);

    if (!operation) {
        LOG_E(CERT_UPDATER_SUBJECT, "Failed creating SubscribeToCertificateUpdatesOperation.");
        return CertSubscribeUpdateStatus::SUBSCRIBE_ERROR_CREATE_OPERATION;
    }

    auto activate = operation->Activate(request, nullptr);
    activate.wait();

    auto responseFuture = operation->GetOperationResult();
    if (responseFuture.wait_for(std::chrono::seconds(SUBSCRIBE_TIMEOUT_SECONDS)) == std::future_status::timeout) {
        LOG_E(CERT_UPDATER_SUBJECT, "Operation timed out while waiting for response from Greengrass Core.");
        return CertSubscribeUpdateStatus::SUBSCRIBE_ERROR_TIMEOUT_RESPONSE;
    }

    auto response = GG::SubscribeToCertificateUpdatesResult(responseFuture.get());
    if (!response) {
        // Handle error.
        auto responseType = response.GetResultType();
        LOG_E(CERT_UPDATER_SUBJECT, "Subscribe failed with response type %d", responseType);
        if (responseType == OPERATION_ERROR) {
            auto *error = response.GetOperationError();
            if (error != nullptr) {
                LOG_E(CERT_UPDATER_SUBJECT, "Cert updates subscribe operation failure response: %s",
                      error->GetMessage().value().c_str());
            }
        } else if (responseType == RPC_ERROR) {
            auto error = response.GetRpcError();
            LOG_E(CERT_UPDATER_SUBJECT, "Cert updates RPC failure response: %s", error.StatusToString().c_str())
        }
        return CertSubscribeUpdateStatus::SUBSCRIBE_ERROR_FAILURE_RESPONSE;
    }

    subscribed = true;
    subscriptionActive->exchange(true);
    LOG_I(CERT_UPDATER_SUBJECT, "Successfully subscribed to cert updates");
    return CertSubscribeUpdateStatus::SUBSCRIBE_SUCCESS;
}

void CertificateUpdater::unsubscribeFromUpdates() {
    subscriptionActive->exchange(false);
    LOG_I(CERT_UPDATER_SUBJECT, "Successfully unsubscribed from cert updates");
}
