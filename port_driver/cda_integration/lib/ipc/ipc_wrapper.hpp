/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#pragma once

#include <aws/crt/Api.h>
#include <aws/greengrass/GreengrassCoreIpcClient.h>

#include "logger.h"

namespace CRT = Aws::Crt;
namespace GG = Aws::Greengrass;

/*
 * Inheriting from ConnectionLifecycleHandler allows us to define callbacks that are
 * called upon when connection lifecycle events occur.
 */
class ConnectionEventsHandler : public ConnectionLifecycleHandler {
public:
    ConnectionEventsHandler() = default;

    void OnConnectCallback() override { LOG("Connected to Greengrass Core"); }

    void OnDisconnectCallback(RpcError status) override {
        LOG("Disconnected from Greengrass Core with status: %s", status.StatusToString().c_str());
    }

    bool OnErrorCallback(RpcError status) override {
        LOG("Processing messages from the Greengrass Core resulted in error: %s", status.StatusToString().c_str());
        // TODO: Should we force close the connection here?
        return true;
    }
};

class GreengrassIPCWrapper {
private:
    CRT::ApiHandle crtApiHandle;
    CRT::Io::ClientBootstrap &clientBootstrap;
    GG::GreengrassCoreIpcClient *ipcClient;
    ConnectionEventsHandler connectionEventsHandler;

    static CRT::Io::ClientBootstrap &getClientBootstrap(CRT::ApiHandle &crtApiHandle);

    void connect();

public:
    GreengrassIPCWrapper(GG::GreengrassCoreIpcClient *client)
            : crtApiHandle(), clientBootstrap(getClientBootstrap(crtApiHandle)) {
        ipcClient = client ? client : new GG::GreengrassCoreIpcClient(clientBootstrap);
        if (!ipcClient) {
            throw std::runtime_error("Failed to create IPC Client");
        }
    };

    ~GreengrassIPCWrapper() {
        delete ipcClient;
    }

    GG::GreengrassCoreIpcClient &getIPCClient();
};

CRT::Io::ClientBootstrap &GreengrassIPCWrapper::getClientBootstrap(CRT::ApiHandle &crtApiHandle) {
    CRT::Io::ClientBootstrap *clientBootstrap = crtApiHandle.GetOrCreateStaticDefaultClientBootstrap();
    if (!clientBootstrap || clientBootstrap->LastError() != AWS_ERROR_SUCCESS) {
        if (clientBootstrap) {
            LOG("ClientBootstrap failed with error %s", CRT::ErrorDebugString(clientBootstrap->LastError()));
        }
        throw std::runtime_error("Unable to get or create default client bootstrap");
    }
    return *clientBootstrap;
}

void GreengrassIPCWrapper::connect() {
    RpcError rpcError = ipcClient->Connect(connectionEventsHandler).get();
    if (!rpcError) {
        LOG("Failed to establish connection IPC. Error %s", rpcError.StatusToString().c_str());
        throw std::runtime_error("Failed to connect to Greengrass Core");
    }
    LOG("Greengrass IPC Client connected successfully!");
}

GG::GreengrassCoreIpcClient &GreengrassIPCWrapper::getIPCClient() {
    return *ipcClient;
};