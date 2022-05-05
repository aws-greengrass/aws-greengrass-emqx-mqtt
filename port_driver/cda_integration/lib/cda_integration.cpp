/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <aws/crt/Api.h>
#include <aws/greengrass/GreengrassCoreIpcClient.h>
#include <functional>
#include <iostream>

#include "cda_integration.h"
#include "logger.h"
#include "ipc/ipc_wrapper.hpp"
#include "cert_generation/certificate_updater.hpp"

class ClientDeviceAuthIntegration {
private:
    GreengrassIPCWrapper greengrassIpcWrapper;
    CertificateUpdater certificateUpdater;

public:
    ClientDeviceAuthIntegration(GG::GreengrassCoreIpcClient *ipcClient)
            : greengrassIpcWrapper(ipcClient), certificateUpdater(greengrassIpcWrapper.getIPCClient()) {};

    [[nodiscard]] bool close() const;

    bool on_client_connect(const char *clientId, const char *pem);

    bool on_client_connected(const char *clientId, const char *pem);

    bool on_client_disconnected(const char *clientId, const char *pem);

    bool on_client_authenticate(const char *clientId, const char *pem);

    bool on_check_acl(const char *clientId, const char *pem, const char *topic, const char *action);

    bool verify_client_certificate(const char *certPem);

    int subscribeToCertUpdates();
};

int ClientDeviceAuthIntegration::subscribeToCertUpdates() {
    return certificateUpdater.subscribeToUpdates();
}

// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
bool ClientDeviceAuthIntegration::close() const { return true; }

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

CDA_INTEGRATION_HANDLE *cda_integration_init(GG::GreengrassCoreIpcClient *client) {
    ClientDeviceAuthIntegration *cda_integ;

    try {
        cda_integ = new ClientDeviceAuthIntegration(client);
    } catch (std::exception &e) {
        LOG("Failed to initialize CDA integration %s", e.what());
    } catch (...) {
        LOG("Failed to initialize CDA integration due to an unknown error");
    }

    try {
        if (cda_integ) {
            int subscribe_return = cda_integ->subscribeToCertUpdates();
            LOG("Retrieved certs from CDA with status: %d", subscribe_return);
            // TODO: Improve return codes
            // TODO: Bubble up the failure in fetching certs
        }
    } catch (std::exception &e) {
        LOG("Failed to retrieve certs. Error: %s", e.what());
    } catch (...) {
        LOG("Failed to retrieve certs due to an unknown error");
    }

    return reinterpret_cast<CDA_INTEGRATION_HANDLE *>(cda_integ);
}

CDA_INTEGRATION_HANDLE *cda_integration_init() { return cda_integration_init(nullptr); }

bool execute_with_handle(CDA_INTEGRATION_HANDLE *handle,
                         const std::function<bool(ClientDeviceAuthIntegration *cda_integ)> &func) {
    if (handle == nullptr) {
        std::cerr << "Handle cannot be null" << std::endl;
        return false;
    }

    auto *cda_integ = reinterpret_cast<ClientDeviceAuthIntegration *>(handle);
    try {
        return func(cda_integ);
    } catch (std::exception &e) {
        std::cerr << e.what() << std::endl;
        return false;
    } catch (...) {
        std::cerr << "Unknown exception" << std::endl;
        return false;
    }
}

bool cda_integration_close(CDA_INTEGRATION_HANDLE *handle) {
    const std::function<bool(ClientDeviceAuthIntegration *cda_integ)> close =
            [](ClientDeviceAuthIntegration *cda_integ) { return cda_integ->close(); };
    return execute_with_handle(handle, close);
}

bool on_client_connect(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem) {
    const std::function<bool(ClientDeviceAuthIntegration *cda_integ)> on_connect =
            [clientId, pem](ClientDeviceAuthIntegration *cda_integ) {
                return cda_integ->on_client_connect(clientId, pem);
            };
    return execute_with_handle(handle, on_connect);
}

bool on_client_connected(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem) {
    const std::function<bool(ClientDeviceAuthIntegration *cda_integ)> on_connected =
            [clientId, pem](ClientDeviceAuthIntegration *cda_integ) {
                return cda_integ->on_client_connected(clientId, pem);
            };
    return execute_with_handle(handle, on_connected);
}

bool on_client_disconnected(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem) {
    const std::function<bool(ClientDeviceAuthIntegration *cda_integ)> on_disconnected =
            [clientId, pem](ClientDeviceAuthIntegration *cda_integ) {
                return cda_integ->on_client_disconnected(clientId, pem);
            };
    return execute_with_handle(handle, on_disconnected);
}

bool on_client_authenticate(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem) {
    const std::function<bool(ClientDeviceAuthIntegration *cda_integ)> on_authenticate =
            [clientId, pem](ClientDeviceAuthIntegration *cda_integ) {
                return cda_integ->on_client_authenticate(clientId, pem);
            };
    return execute_with_handle(handle, on_authenticate);
}

bool on_check_acl(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem, const char *topic,
                  const char *action) {
    const std::function<bool(ClientDeviceAuthIntegration *cda_integ)> on_check_acl_func =
            [clientId, pem, topic, action](ClientDeviceAuthIntegration *cda_integ) {
                return cda_integ->on_check_acl(clientId, pem, topic, action);
            };
    return execute_with_handle(handle, on_check_acl_func);
}

bool verify_client_certificate(CDA_INTEGRATION_HANDLE *handle, const char *certPem) {
    const std::function<bool(ClientDeviceAuthIntegration *cda_integ)> verify_client_cert_fun =
            [certPem](ClientDeviceAuthIntegration *cda_integ) { return cda_integ->verify_client_certificate(certPem); };
    return execute_with_handle(handle, verify_client_cert_fun);
}
