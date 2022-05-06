/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <aws/crt/Api.h>
#include <aws/crt/io/HostResolver.h>
#include <aws/greengrass/GreengrassCoreIpcClient.h>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iostream>

#include "cda_integration.h"

using namespace Aws::Crt;
using namespace Aws::Greengrass;
using namespace std;

const filesystem::path EMQX_KEY_PATH = filesystem::path{"etc/greengrass_certs/greengrass_emqx.key"};
const filesystem::path EMQX_PEM_PATH = filesystem::path{"etc/greengrass_certs/greengrass_emqx.pem"};
const filesystem::path EMQX_CA_PATH = filesystem::path{"etc/greengrass_certs/greengrass_ca.pem"};

enum log_subject {
    CDA_INTEG_SUBJECT = AWS_LOG_SUBJECT_BEGIN_RANGE(101),
};

#define LOG(...) AWS_LOGF_INFO(CDA_INTEG_SUBJECT, __VA_ARGS__)

class ClientDeviceAuthIntegration {
  private:
    unique_ptr<GreengrassCoreIpcClient> ipcClient;
    unique_ptr<ApiHandle> apiHandle;

  public:
    ClientDeviceAuthIntegration();

    bool close() const;

    bool on_client_connect(const char *clientId, const char *pem);

    bool on_client_connected(const char *clientId, const char *pem);

    bool on_client_disconnected(const char *clientId, const char *pem);

    bool on_client_authenticate(const char *clientId, const char *pem);

    bool on_check_acl(const char *clientId, const char *pem, const char *topic, const char *action);

    bool verify_client_certificate(const char *certPem);

    int retrieveCertsFromCda();
};

/*
 * Inheriting from ConnectionLifecycleHandler allows us to define callbacks that are
 * called upon when connection lifecycle events occur.
 */
class TestConnectionLifecycleHandler : public ConnectionLifecycleHandler {
  public:
    TestConnectionLifecycleHandler() = default;
    void OnConnectCallback() override { LOG("Connected to Greengrass Core"); }
    void OnDisconnectCallback(RpcError status) override {
        if (!status) {
            LOG("Disconnected from Greengrass Core with error: %s", status.StatusToString().c_str());
            exit(-1);
        }
    }
    bool OnErrorCallback(RpcError status) override {
        LOG("Processing messages from the Greengrass Core resulted in error: %s", status.StatusToString().c_str());
        return true;
    }
};

int ClientDeviceAuthIntegration::retrieveCertsFromCda() {

    class CertificateUpdatesStreamHandler : public SubscribeToCertificateUpdatesStreamHandler {
        void OnStreamEvent(CertificateUpdateEvent *response) override {
            LOG("Retrieving all certs...");
            auto certUpdate = response->GetCertificateUpdate();
            auto privateKey = certUpdate->GetPrivateKey();
            auto cert = certUpdate->GetCertificate();
            auto allCas = certUpdate->GetCaCertificates();
            LOG("Retrieved all certs from response...");

            auto cwd = std::filesystem::current_path();
            LOG("Current working directory is %s", cwd.c_str());
            ofstream out_key(cwd / EMQX_KEY_PATH);
            out_key << privateKey.value().c_str();
            out_key.close();
            ofstream out_pem(cwd / EMQX_PEM_PATH);
            out_pem << cert.value().c_str();
            out_pem.close();
            ofstream out_ca(cwd / EMQX_CA_PATH);
            out_ca << allCas.value().front().c_str();
            out_ca.close();
            LOG("Wrote all certs!");
        }

        bool OnStreamError(OperationError *error) override {
            LOG("OnStream error %s", error->GetMessage().value().c_str());
            return false; // Return true to close stream, false to keep stream open.
        }

        void OnStreamClosed() override {
            LOG("Stream closed");
            // Handle close.
        }
    };

    SubscribeToCertificateUpdatesRequest request;
    auto options = std::make_unique<CertificateOptions>();
    options->SetCertificateType(CERTIFICATE_TYPE_SERVER);
    request.SetCertificateOptions(*options);

    shared_ptr<CertificateUpdatesStreamHandler> streamHandler(new CertificateUpdatesStreamHandler());
    auto operation = ipcClient->NewSubscribeToCertificateUpdates(streamHandler);
    auto activate = operation->Activate(request, nullptr);
    activate.wait();

    auto responseFuture = operation->GetResult();
    if (responseFuture.wait_for(std::chrono::seconds(10)) == std::future_status::timeout) {
        LOG("Operation timed out while waiting for response from Greengrass Core.");
        exit(-1);
    }
    auto response = responseFuture.get();
    LOG("Received response from CDA...");
    if (!response) {
        LOG("Empty response");
        // Handle error.
        auto errorType = response.GetResultType();
        LOG("Subscribe error %d", errorType);
        if (errorType == OPERATION_ERROR) {
            auto *error = response.GetOperationError();
            LOG("Cert subscribe response error %s", error->GetMessage().value().c_str());
        } else {
            // Handle RPC error.
        }
        return -1;
    }
    LOG("Done subscribing");
    return 0;
}

ClientDeviceAuthIntegration::ClientDeviceAuthIntegration() {
    LOG("Attempting to initialize Greengrass IPC client...");
    apiHandle = std::make_unique<ApiHandle>(g_allocator);
    if (apiHandle->GetOrCreateStaticDefaultClientBootstrap()->LastError() != AWS_ERROR_SUCCESS) {
        LOG("ClientBootstrap failed with error %s",
            ErrorDebugString(apiHandle->GetOrCreateStaticDefaultClientBootstrap()->LastError()));
        exit(-1);
    }
    ipcClient = std::make_unique<GreengrassCoreIpcClient>(*apiHandle->GetOrCreateStaticDefaultClientBootstrap());

    TestConnectionLifecycleHandler lifecycleHandler;
    auto connectionStatus = ipcClient->Connect(lifecycleHandler).get();
    if (!connectionStatus) {
        LOG("Failed to establish connection with error %s", connectionStatus.StatusToString().c_str());
        exit(-1);
    }
    LOG("Greengrass IPC Client created and connected successfully!\n");
    int subscribe_return = ClientDeviceAuthIntegration::retrieveCertsFromCda();
    LOG("Retrieved certs from CDA with status: %d", subscribe_return);
}

bool ClientDeviceAuthIntegration::close() const { return true; }

bool ClientDeviceAuthIntegration::on_client_connect(const char *clientId, const char *pem) {
    std::cout << "on_client_connect called with clientId: " << clientId << " and pem: " << pem << std::endl;
    return true;
}

bool ClientDeviceAuthIntegration::on_client_connected(const char *clientId, const char *pem) {
    std::cout << "on_client_connected called with clientId: " << clientId << " and pem: " << pem << std::endl;
    return true;
}

bool ClientDeviceAuthIntegration::on_client_disconnected(const char *clientId, const char *pem) {
    std::cout << "on_client_disconnected called with clientId: " << clientId << " and pem: " << pem << std::endl;
    return true;
}

bool ClientDeviceAuthIntegration::on_client_authenticate(const char *clientId, const char *pem) {
    std::cout << "on_client_authenticate called with clientId: " << clientId << " and pem: " << pem << std::endl;
    return true;
}

bool ClientDeviceAuthIntegration::on_check_acl(const char *clientId, const char *pem, const char *topic,
                                               const char *action) {
    std::cout << "on_check_acl called with clientId: " << clientId << " and pem: " << pem << " and topic: " << topic
              << " and action: " << action << std::endl;
    return true;
}

bool ClientDeviceAuthIntegration::verify_client_certificate(const char *certPem) {
    std::cout << "verify_client_certificate called with certPem: " << certPem << std::endl;
    return true;
}

CDA_INTEGRATION_HANDLE *cda_integration_init() {
    ClientDeviceAuthIntegration *cda_integ = nullptr;

    try {
        cda_integ = new ClientDeviceAuthIntegration();
    } catch (std::exception &e) {
        std::cerr << e.what() << std::endl;
    } catch (...) {
        std::cerr << "Unknown exception" << std::endl;
    }

    return reinterpret_cast<CDA_INTEGRATION_HANDLE *>(cda_integ);
}

bool execute_with_handle(CDA_INTEGRATION_HANDLE *handle,
                         std::function<bool(ClientDeviceAuthIntegration *cda_integ)> func) {
    if (!handle) {
        std::cerr << "Handle cannot be null" << std::endl;
        return false;
    }

    ClientDeviceAuthIntegration *cda_integ = reinterpret_cast<ClientDeviceAuthIntegration *>(handle);
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
    const std::function<bool(ClientDeviceAuthIntegration * cda_integ)> close =
        [](ClientDeviceAuthIntegration *cda_integ) { return cda_integ->close(); };
    return execute_with_handle(handle, close);
}

bool on_client_connect(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem) {
    const std::function<bool(ClientDeviceAuthIntegration * cda_integ)> on_connect =
        [clientId, pem](ClientDeviceAuthIntegration *cda_integ) { return cda_integ->on_client_connect(clientId, pem); };
    return execute_with_handle(handle, on_connect);
}

bool on_client_connected(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem) {
    const std::function<bool(ClientDeviceAuthIntegration * cda_integ)> on_connected =
        [clientId, pem](ClientDeviceAuthIntegration *cda_integ) {
            return cda_integ->on_client_connected(clientId, pem);
        };
    return execute_with_handle(handle, on_connected);
}

bool on_client_disconnected(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem) {
    const std::function<bool(ClientDeviceAuthIntegration * cda_integ)> on_disconnected =
        [clientId, pem](ClientDeviceAuthIntegration *cda_integ) {
            return cda_integ->on_client_disconnected(clientId, pem);
        };
    return execute_with_handle(handle, on_disconnected);
}

bool on_client_authenticate(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem) {
    const std::function<bool(ClientDeviceAuthIntegration * cda_integ)> on_authenticate =
        [clientId, pem](ClientDeviceAuthIntegration *cda_integ) {
            return cda_integ->on_client_authenticate(clientId, pem);
        };
    return execute_with_handle(handle, on_authenticate);
}

bool on_check_acl(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem, const char *topic,
                  const char *action) {
    const std::function<bool(ClientDeviceAuthIntegration * cda_integ)> on_check_acl_func =
        [clientId, pem, topic, action](ClientDeviceAuthIntegration *cda_integ) {
            return cda_integ->on_check_acl(clientId, pem, topic, action);
        };
    return execute_with_handle(handle, on_check_acl_func);
}

bool verify_client_certificate(CDA_INTEGRATION_HANDLE *handle, const char *certPem) {
    const std::function<bool(ClientDeviceAuthIntegration * cda_integ)> verify_client_cert_fun =
        [certPem](ClientDeviceAuthIntegration *cda_integ) { return cda_integ->verify_client_certificate(certPem); };
    return execute_with_handle(handle, verify_client_cert_fun);
}
