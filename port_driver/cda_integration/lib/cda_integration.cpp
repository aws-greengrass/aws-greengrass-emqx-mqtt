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

using namespace Aws::Crt;
using namespace Aws::Greengrass;
using namespace std;

class ClientDeviceAuthIntegration {
private:
    unique_ptr<GreengrassCoreIpcClient> ipcClient;
public:
    ClientDeviceAuthIntegration();

    bool close() const;

    bool on_client_connect(const char *clientId, const char *pem);

    bool on_client_connected(const char *clientId, const char *pem);

    bool on_client_disconnected(const char *clientId, const char *pem);

    bool on_client_authenticate(const char *clientId, const char *pem);

    bool on_check_acl(const char *clientId, const char *pem,
                      const char *topic, const char *action);

    unique_ptr<GreengrassCoreIpcClient> createIpcClient();

    int test_publish();

};

/*
 * Inheriting from ConnectionLifecycleHandler allows us to define callbacks that are
 * called upon when connection lifecycle events occur.
 */
class SampleLifecycleHandler : public ConnectionLifecycleHandler{
    public:
        SampleLifecycleHandler() {}
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

unique_ptr<GreengrassCoreIpcClient> ClientDeviceAuthIntegration::createIpcClient(){

    fprintf(stdout, "avipinku testconnect hippo");

    /************************ Setup the Lib ****************************/
    /*
     * Do the global initialization for the API.
     */
     ApiHandle apiHandle;
     if (apiHandle.GetOrCreateStaticDefaultClientBootstrap()->LastError() != AWS_ERROR_SUCCESS){
        fprintf(
            stderr,
            "ClientBootstrap failed with error %s\n",
            ErrorDebugString(apiHandle.GetOrCreateStaticDefaultClientBootstrap()->LastError()));
        exit(-1);
    }

    /*
     * Note: The lifecycle handler should be declared before the client
     * so that it is destroyed AFTER the client is destroyed.
     */
    SampleLifecycleHandler lifecycleHandler;
    unique_ptr<GreengrassCoreIpcClient> client(new GreengrassCoreIpcClient(*apiHandle.GetOrCreateStaticDefaultClientBootstrap()));
    auto connectionStatus = client->Connect(lifecycleHandler).get();

    if (!connectionStatus)
    {
        fprintf(stderr, "Failed to establish connection with error %s\n", connectionStatus.StatusToString().c_str());
        exit(-1);
    }

    return client;
}


int ClientDeviceAuthIntegration::test_publish(){
    std::cout << "test_publish() called lion!" << std::endl;
    /* Publish to the same topic that is currently subscribed to. */
     String topic =  "test/topic";
     String message = "Hello World";

    fprintf(stdout, "Attempting to reuse IPC client \n");
    auto publishOperation = ipcClient->NewPublishToIoTCore();
    fprintf(stdout, "Created publish operation \n");
    PublishToIoTCoreRequest publishRequest;
    publishRequest.SetTopicName(topic);
    Vector<uint8_t> payload(message.begin(), message.end());
    publishRequest.SetPayload(payload);
    publishRequest.SetQos(QOS_AT_LEAST_ONCE);

    fprintf(stdout, "Attempting to publish to %s topic\n", topic.c_str());
    auto requestStatus = publishOperation->Activate(publishRequest).get();
    if (!requestStatus)
    {
        fprintf(
            stderr,
            "Failed to publish to %s topic with error %s\n",
            topic.c_str(),
            requestStatus.StatusToString().c_str());
        exit(-1);
    }

    auto publishResultFuture = publishOperation->GetResult();
    /*
    // To avoid throwing exceptions, wait on the result for a specified timeout:
    if (publishResultFuture.wait_for(std::chrono::seconds(10)) == std::future_status::timeout)
    {
        fprintf(stderr, "Timed out while waiting for response from Greengrass Core\n");
        exit(-1);
    }
    */

    auto publishResult = publishResultFuture.get();
    if (publishResult)
    {
        fprintf(stdout, "Successfully published to %s topic\n", topic.c_str());
        auto *response = publishResult.GetOperationResponse();
        (void)response;
    }
    else
    {
        auto errorType = publishResult.GetResultType();
        if (errorType == OPERATION_ERROR)
        {
            OperationError *error = publishResult.GetOperationError();
            /*
             * This pointer can be casted to any error type like so:
             * if(error->GetModelName() == UnauthorizedError::MODEL_NAME)
             *    UnauthorizedError *unauthorizedError = static_cast<UnauthorizedError*>(error);
             */
            if (error->GetMessage().has_value())
                fprintf(stderr, "Greengrass Core responded with an error: %s\n", error->GetMessage().value().c_str());
        }
        else
        {
            fprintf(
                stderr,
                "Attempting to receive the response from the server failed with error code %s\n",
                publishResult.GetRpcError().StatusToString().c_str());
        }
    }
    return 0;
}

ClientDeviceAuthIntegration::ClientDeviceAuthIntegration() {
    try{
        ipcClient = ClientDeviceAuthIntegration::createIpcClient();
        std::cout << "client created~~" << std::endl;
        int publish_return = ClientDeviceAuthIntegration::test_publish();
        std::cout << "on_client_connect called avipinku2: " << publish_return << std::endl;
    } catch(exception& e){
        std::cerr << "caught exception! " << e.what() << std::endl;
    }
}

bool ClientDeviceAuthIntegration::close() const {
    return true;
}


bool ClientDeviceAuthIntegration::on_client_connect(const char* clientId, const char* pem) {
    std::cout << "on_client_connect called with clientId: " << clientId << " and pem: "<< pem << std::endl;
    int publish_return = ClientDeviceAuthIntegration::test_publish();
    std::cout << "on_client_connect called avipinku66666: " << publish_return << std::endl;
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

bool ClientDeviceAuthIntegration::on_check_acl(const char *clientId, const char *pem,
                                               const char *topic,
                                               const char *action) {
    std::cout << "on_check_acl called with clientId: " << clientId << " and pem: " << pem << " and topic: " << topic <<
              " and action: " << action << std::endl;
    return true;
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

bool
execute_with_handle(CDA_INTEGRATION_HANDLE *handle, std::function<bool(ClientDeviceAuthIntegration *cda_integ)> func) {
    if (!handle) {
        std::cerr << "Handle cannot be null" << std::endl;
        return false;
    }

    ClientDeviceAuthIntegration *cda_integ = reinterpret_cast<ClientDeviceAuthIntegration *>(handle);
    try {
        return func(cda_integ);
    }
    catch (std::exception &e) {
        std::cerr << e.what() << std::endl;
        return false;
    }
    catch (...) {
        std::cerr << "Unknown exception" << std::endl;
        return false;
    }
}

bool cda_integration_close(CDA_INTEGRATION_HANDLE *handle) {
    const std::function<bool(ClientDeviceAuthIntegration *cda_integ)> close = [](ClientDeviceAuthIntegration *
    cda_integ) {
        return cda_integ->close();
    };
    return execute_with_handle(handle, close);
}

bool on_client_connect(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem) {
    const std::function<bool(ClientDeviceAuthIntegration *cda_integ)> on_connect = [clientId, pem]
            (ClientDeviceAuthIntegration *cda_integ) {
        return cda_integ->on_client_connect(clientId, pem);
    };
    return execute_with_handle(handle, on_connect);
}

bool on_client_connected(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem) {
    const std::function<bool(ClientDeviceAuthIntegration *cda_integ)> on_connected = [clientId, pem]
            (ClientDeviceAuthIntegration *cda_integ) {
        return cda_integ->on_client_connected(clientId, pem);
    };
    return execute_with_handle(handle, on_connected);
}

bool on_client_disconnected(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem) {
    const std::function<bool(ClientDeviceAuthIntegration *cda_integ)> on_disconnected = [clientId, pem]
            (ClientDeviceAuthIntegration *cda_integ) {
        return cda_integ->on_client_disconnected(clientId, pem);
    };
    return execute_with_handle(handle, on_disconnected);
}

bool on_client_authenticate(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem) {
    const std::function<bool(ClientDeviceAuthIntegration *cda_integ)> on_authenticate = [clientId, pem]
            (ClientDeviceAuthIntegration *cda_integ) {
        return cda_integ->on_client_authenticate(clientId, pem);
    };
    return execute_with_handle(handle, on_authenticate);
}

bool on_check_acl(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem,
                  const char *topic, const char *action) {
    const std::function<bool(ClientDeviceAuthIntegration *cda_integ)> on_check_acl_func =
            [clientId, pem, topic, action](ClientDeviceAuthIntegration *cda_integ) {
                return cda_integ->on_check_acl(clientId, pem, topic, action);
            };
    return execute_with_handle(handle, on_check_acl_func);
}


