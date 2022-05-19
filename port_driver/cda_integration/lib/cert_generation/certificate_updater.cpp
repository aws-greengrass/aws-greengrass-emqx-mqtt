/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <aws/greengrass/GreengrassCoreIpcClient.h>
#include <filesystem>
#include "logger.h"
#include "private/certificate_updater.h"

#define SUBSCRIBE_TIMEOUT_SECONDS 10

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
        return -2;
    }

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
            // TODO: Improve error handling
        }
        return -3;
    }
    LOG_I(CERT_UPDATER_SUBJECT, "Successfully subscribed to cert updates");
    return 0;
}
