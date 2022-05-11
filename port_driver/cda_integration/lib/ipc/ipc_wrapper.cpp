/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <aws/crt/Api.h>
#include <aws/greengrass/GreengrassCoreIpcClient.h>

#include "logger.h"
#include "private/ipc_wrapper.h"

void ConnectionEventsHandler::OnConnectCallback() { LOG("Connected to Greengrass Core"); }

void ConnectionEventsHandler::OnDisconnectCallback(RpcError status) {
    LOG("Disconnected from Greengrass Core with status: %s", status.StatusToString().c_str());
}

bool ConnectionEventsHandler::OnErrorCallback(RpcError status) {
    LOG("Processing messages from the Greengrass Core resulted in error: %s", status.StatusToString().c_str());
    // TODO: Should we force close the connection here?
    return true;
}

GreengrassIPCWrapper::GreengrassIPCWrapper(GG::GreengrassCoreIpcClient *client)
    : clientBootstrap(getClientBootstrap()) {
    ipcClient = client != nullptr ? client : new GG::GreengrassCoreIpcClient(clientBootstrap);
    if (ipcClient == nullptr) {
        throw std::runtime_error("Failed to create IPC Client");
    }
};

GreengrassIPCWrapper::~GreengrassIPCWrapper() { delete ipcClient; }

CRT::Io::ClientBootstrap &GreengrassIPCWrapper::getClientBootstrap() {
    CRT::Io::ClientBootstrap *clientBootstrap = CRT::ApiHandle::GetOrCreateStaticDefaultClientBootstrap();
    if (clientBootstrap == nullptr || clientBootstrap->LastError() != AWS_ERROR_SUCCESS) {
        if (clientBootstrap != nullptr) {
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

GG::GreengrassCoreIpcClient &GreengrassIPCWrapper::getIPCClient() { return *ipcClient; }
