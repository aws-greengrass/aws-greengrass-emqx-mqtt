/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <iostream>
#include <functional>
#include <aws/crt/Api.h>
#include <aws/crt/io/HostResolver.h>
#include <aws/greengrass/GreengrassCoreIpcClient.h>

#include "cda_integration.h"

class ClientDeviceAuthIntegration {
public:
    ClientDeviceAuthIntegration();

    OperationResult close() const;

    OperationResult on_client_connect(const char *clientId, const char *pem);

    OperationResult on_client_connected(const char *clientId, const char *pem);

    OperationResult on_client_disconnected(const char *clientId, const char *pem);

    OperationResult on_client_authenticate(const char *clientId, const char *pem);

    OperationResult on_check_acl(const char *clientId, const char *pem,
                      const char *topic, const char *action);
    
    OperationResult verify_client_certificate(const char* certPem);
};

ClientDeviceAuthIntegration::ClientDeviceAuthIntegration() {
}

OperationResult ClientDeviceAuthIntegration::close() const {
    return OperationResult::PASS;
}

OperationResult ClientDeviceAuthIntegration::on_client_connect(const char *clientId, const char *pem) {
    std::cout << "on_client_connect called with clientId: " << clientId << " and pem: " << pem << std::endl;
    return OperationResult::PASS;
}

OperationResult ClientDeviceAuthIntegration::on_client_connected(const char *clientId, const char *pem) {
    std::cout << "on_client_connected called with clientId: " << clientId << " and pem: " << pem << std::endl;
    return OperationResult::PASS;
}

OperationResult ClientDeviceAuthIntegration::on_client_disconnected(const char *clientId, const char *pem) {
    std::cout << "on_client_disconnected called with clientId: " << clientId << " and pem: " << pem << std::endl;
    return OperationResult::PASS;
}

OperationResult ClientDeviceAuthIntegration::on_client_authenticate(const char *clientId, const char *pem) {
    std::cout << "on_client_authenticate called with clientId: " << clientId << " and pem: " << pem << std::endl;
    return OperationResult::PASS;
}

OperationResult ClientDeviceAuthIntegration::on_check_acl(const char *clientId, const char *pem,
                                               const char *topic,
                                               const char *action) {
    std::cout << "on_check_acl called with clientId: " << clientId << " and pem: " << pem << " and topic: " << topic <<
              " and action: " << action << std::endl;
    return OperationResult::PASS;
}

OperationResult ClientDeviceAuthIntegration::verify_client_certificate(const char *certPem) {
    std::cout << "verify_client_certificate called with certPem: " << certPem << std::endl;
    return OperationResult::PASS;
}

CDA_INTEGRATION_HANDLE *cda_integration_init() {
    ClientDeviceAuthIntegration *cda_integ = nullptr;

    try {
        cda_integ = new ClientDeviceAuthIntegration();
    }
    catch (std::exception &e) {
        std::cerr << e.what() << std::endl;
    }
    catch (...) {
        std::cerr << "Unknown exception" << std::endl;
    }

    return reinterpret_cast<CDA_INTEGRATION_HANDLE *>(cda_integ);
}

OperationResult
execute_with_handle(CDA_INTEGRATION_HANDLE *handle, std::function<OperationResult(ClientDeviceAuthIntegration *cda_integ)> func) {
    if (!handle) {
        std::cerr << "Handle cannot be null" << std::endl;
        return OperationResult::FAIL;
    }

    ClientDeviceAuthIntegration *cda_integ = reinterpret_cast<ClientDeviceAuthIntegration *>(handle);
    try {
        return func(cda_integ);
    }
    catch (std::exception &e) {
        std::cerr << e.what() << std::endl;
        return OperationResult::FAIL;
    }
    catch (...) {
        std::cerr << "Unknown exception" << std::endl;
        return OperationResult::FAIL;
    }
}

OperationResult cda_integration_close(CDA_INTEGRATION_HANDLE *handle) {
    const std::function<OperationResult(ClientDeviceAuthIntegration *cda_integ)> close = [](ClientDeviceAuthIntegration *
    cda_integ) {
        return cda_integ->close();
    };
    return execute_with_handle(handle, close);
}

OperationResult on_client_connect(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem) {
    const std::function<OperationResult(ClientDeviceAuthIntegration *cda_integ)> on_connect = [clientId, pem]
            (ClientDeviceAuthIntegration *cda_integ) {
        return cda_integ->on_client_connect(clientId, pem);
    };
    return execute_with_handle(handle, on_connect);
}

OperationResult on_client_connected(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem) {
    const std::function<OperationResult(ClientDeviceAuthIntegration *cda_integ)> on_connected = [clientId, pem]
            (ClientDeviceAuthIntegration *cda_integ) {
        return cda_integ->on_client_connected(clientId, pem);
    };
    return execute_with_handle(handle, on_connected);
}

OperationResult on_client_disconnected(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem) {
    const std::function<OperationResult(ClientDeviceAuthIntegration *cda_integ)> on_disconnected = [clientId, pem]
            (ClientDeviceAuthIntegration *cda_integ) {
        return cda_integ->on_client_disconnected(clientId, pem);
    };
    return execute_with_handle(handle, on_disconnected);
}

OperationResult on_client_authenticate(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem) {
    const std::function<OperationResult(ClientDeviceAuthIntegration *cda_integ)> on_authenticate = [clientId, pem]
            (ClientDeviceAuthIntegration *cda_integ) {
        return cda_integ->on_client_authenticate(clientId, pem);
    };
    return execute_with_handle(handle, on_authenticate);
}

OperationResult on_check_acl(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem,
                  const char *topic, const char *action) {
    const std::function<OperationResult(ClientDeviceAuthIntegration *cda_integ)> on_check_acl_func =
            [clientId, pem, topic, action](ClientDeviceAuthIntegration *cda_integ) {
                return cda_integ->on_check_acl(clientId, pem, topic, action);
            };
    return execute_with_handle(handle, on_check_acl_func);
}

OperationResult verify_client_certificate(CDA_INTEGRATION_HANDLE* handle, const char* certPem) {
    const std::function<OperationResult(ClientDeviceAuthIntegration* cda_integ)> verify_client_certificate_func =
    [certPem] (ClientDeviceAuthIntegration* cda_integ) {
        return cda_integ->verify_client_certificate(certPem);
    };
    return execute_with_handle(handle, verify_client_certificate_func);
}

