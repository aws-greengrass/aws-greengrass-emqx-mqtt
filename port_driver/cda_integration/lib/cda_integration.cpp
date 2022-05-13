/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <aws/greengrass/GreengrassCoreIpcClient.h>
#include <filesystem>
#include <functional>
#include <iostream>

#include "cda_integration.h"

void ClientDeviceAuthIntegration::connect() { greengrassIpcWrapper.connect(); }

int ClientDeviceAuthIntegration::subscribeToCertUpdates(
    std::unique_ptr<std::filesystem::path> basePath,
    std::unique_ptr<std::function<void(GG::CertificateUpdateEvent *)>> subscription) {
    int certSubscribeStatus = certificateUpdater.subscribeToUpdates(std::move(basePath), std::move(subscription));
    if (certSubscribeStatus != 0) {
        LOG_E(CDA_INTEG_SUBJECT, "Failed to subscribe to cert updates with status %d", certSubscribeStatus);
        return certSubscribeStatus;
    }
    LOG_I(CDA_INTEG_SUBJECT, "Certs were successfully retrieved from Greengrass IPC");
    return 0;
}

// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
bool ClientDeviceAuthIntegration::on_client_connect(const char *clientId, const char *pem) {
    std::cout << "on_client_connect called with clientId: " << clientId << " and pem: " << pem << std::endl;
    return true;
}

// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
bool ClientDeviceAuthIntegration::on_client_connected(const char *clientId, const char *pem) {
    std::cout << "on_client_connected called with clientId: " << clientId << " and pem: " << pem << std::endl;
    return true;
}

// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
bool ClientDeviceAuthIntegration::on_client_disconnected(const char *clientId, const char *pem) {
    std::cout << "on_client_disconnected called with clientId: " << clientId << " and pem: " << pem << std::endl;
    return true;
}

// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
bool ClientDeviceAuthIntegration::on_client_authenticate(const char *clientId, const char *pem) {
    std::cout << "on_client_authenticate called with clientId: " << clientId << " and pem: " << pem << std::endl;
    return true;
}

// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
bool ClientDeviceAuthIntegration::on_check_acl(const char *clientId, const char *pem, const char *topic,
                                               const char *action) {
    std::cout << "on_check_acl called with clientId: " << clientId << " and pem: " << pem << " and topic: " << topic
              << " and action: " << action << std::endl;
    return true;
}

// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
bool ClientDeviceAuthIntegration::verify_client_certificate(const char *certPem) {
    std::cout << "verify_client_certificate called with certPem: " << certPem << std::endl;
    return true;
}

ClientDeviceAuthIntegration *cda_integration_init(GG::GreengrassCoreIpcClient *client) {
    ClientDeviceAuthIntegration *cda_integ = nullptr;
    try {
        cda_integ = new ClientDeviceAuthIntegration(client);
    } catch (std::exception &e) {
        LOG_E(CDA_INTEG_SUBJECT, "Failed to initialize CDA integration %s", e.what());
    } catch (...) {
        LOG_E(CDA_INTEG_SUBJECT, "Failed to initialize CDA integration due to an unknown error");
    }
    return cda_integ;
}

ClientDeviceAuthIntegration *cda_integration_init() { return cda_integration_init(nullptr); }

void cda_integration_close(ClientDeviceAuthIntegration *cda_integ) { delete cda_integ; }
