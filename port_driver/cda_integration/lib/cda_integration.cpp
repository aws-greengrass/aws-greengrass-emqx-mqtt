/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <filesystem>
#include <fstream>
#include <string>
#include <aws/crt/Api.h>
#include <aws/crt/io/HostResolver.h>
#include <aws/greengrass/GreengrassCoreIpcClient.h>
#include <functional>
#include <iostream>

#include "cda_integration.h"

using namespace Aws::Crt;
using namespace Aws::Greengrass;
using namespace std;

const string EMQX_KEY_PATH = "/etc/greengrass_certs/greengrass_emqx.key";
const string EMQX_PEM_PATH = "/etc/greengrass_certs/greengrass_emqx.pem";
const string EMQX_CA_PATH = "/etc/greengrass_certs/greengrass_ca.pem";

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
class TestConnectionLifecycleHandler : public ConnectionLifecycleHandler{
    public:
        TestConnectionLifecycleHandler() {}
        void OnConnectCallback() override { fprintf(stdout, "Connected to Greengrass Core\n"); }
        void OnDisconnectCallback(RpcError status) override{
            if (!status){
                fprintf(stdout, "Disconnected from Greengrass Core with error: %s\n", status.StatusToString().c_str());
                exit(-1);
            }
        }
        bool OnErrorCallback(RpcError status) override {
            fprintf(
                stdout,
                "Processing messages from the Greengrass Core resulted in error: %s\n",
                status.StatusToString().c_str());
            return true;
        }
};

int ClientDeviceAuthIntegration::retrieveCertsFromCda(){

    class CertificateUpdatesStreamHandler : public SubscribeToCertificateUpdatesStreamHandler {
        void OnStreamEvent(CertificateUpdateEvent *response) override {
            fprintf(stdout, "Retrieving all certs...\n");
            auto certUpdate = response->GetCertificateUpdate();
            auto privateKey = certUpdate->GetPrivateKey();
            auto cert = certUpdate->GetCertificate();
            auto allCas = certUpdate->GetCaCertificates();
            fprintf(stdout, "Retrieved all certs from response...\n");

            auto cwd = std::filesystem::current_path().string();
            fprintf(stdout, "Current working directory is: %s \n", cwd.c_str());
            ofstream out_key(cwd + EMQX_KEY_PATH);
            out_key << privateKey.value().c_str();
            out_key.close();
            ofstream out_pem(cwd + EMQX_PEM_PATH);
            out_pem << cert.value().c_str();
            out_pem.close();
            ofstream out_ca(cwd + EMQX_CA_PATH);
            out_ca << allCas.value().front().c_str();
            out_ca.close();
            fprintf(stdout, "Wrote all certs!\n");
        }

        bool OnStreamError(OperationError *error) override {
            fprintf(stderr, "OnStream error %s\n", error->GetMessage().value().c_str());
            return false; // Return true to close stream, false to keep stream open.
        }

        void OnStreamClosed() override {
            fprintf(stdout, "Stream closed\n");
            // Handle close.
        }
    };

    SubscribeToCertificateUpdatesRequest request;
    CertificateOptions* options = new CertificateOptions();
    options->SetCertificateType(CERTIFICATE_TYPE_SERVER);
    request.SetCertificateOptions(*options);

    shared_ptr<CertificateUpdatesStreamHandler> streamHandler(new CertificateUpdatesStreamHandler());
    auto operation = ipcClient->NewSubscribeToCertificateUpdates(streamHandler);
    auto activate = operation->Activate(request, nullptr);
    activate.wait();

    auto responseFuture = operation->GetResult();
    if (responseFuture.wait_for(std::chrono::seconds(10)) == std::future_status::timeout) {
        std::cerr << "Operation timed out while waiting for response from Greengrass Core." << std::endl;
        exit(-1);
    }
    auto response = responseFuture.get();
    fprintf(stdout, "Received response from CDA...\n");
    if (!response) {
        fprintf(stderr, "Empty response\n");
        // Handle error.
        auto errorType = response.GetResultType();
        fprintf(stdout, "Subscribe error %d\n", errorType);
        if (errorType == OPERATION_ERROR) {
            auto *error = response.GetOperationError();
            fprintf(stderr, "Cert subscribe response error %s\n", error->GetMessage().value().c_str());
        } else {
            // Handle RPC error.
        }
        return -1;
    }
    fprintf(stdout, "Done subscribing\n");
    return 0;
}

ClientDeviceAuthIntegration::ClientDeviceAuthIntegration() {
    fprintf(stdout, "Attempting to initialize Greengrass IPC client...\n" );
    apiHandle = unique_ptr<ApiHandle>(new ApiHandle{g_allocator});
    if (apiHandle->GetOrCreateStaticDefaultClientBootstrap()->LastError() != AWS_ERROR_SUCCESS){
        fprintf(
            stderr,
            "ClientBootstrap failed with error %s\n",
            ErrorDebugString(apiHandle->GetOrCreateStaticDefaultClientBootstrap()->LastError()));
        exit(-1);
    }
    ipcClient = unique_ptr<GreengrassCoreIpcClient>(new GreengrassCoreIpcClient(*apiHandle->GetOrCreateStaticDefaultClientBootstrap()));

    TestConnectionLifecycleHandler lifecycleHandler;
    auto connectionStatus = ipcClient->Connect(lifecycleHandler).get();
    if (!connectionStatus)
    {
        fprintf(stderr, "Failed to establish connection with error %s\n", connectionStatus.StatusToString().c_str());
        exit(-1);
    }
    fprintf(stdout, "Greengrass IPC Client created and connected successfully!\n" );
    int subscribe_return = ClientDeviceAuthIntegration::retrieveCertsFromCda();
    std::cout << "Retrieved certs from CDA with status : " << subscribe_return << std::endl;
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
