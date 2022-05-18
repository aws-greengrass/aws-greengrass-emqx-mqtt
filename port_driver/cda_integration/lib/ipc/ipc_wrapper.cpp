/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include "private/ipc_wrapper.h"
#include "logger.h"
#include <aws/crt/Api.h>
#include <aws/greengrass/GreengrassCoreIpcClient.h>

#define TIMEOUT_SECONDS 10

void ConnectionEventsHandler::OnConnectCallback() { LOG_I(IPC_WRAPPER_SUBJECT, "Connected to Greengrass Core"); }

void ConnectionEventsHandler::OnDisconnectCallback(RpcError status) {
    LOG_W(IPC_WRAPPER_SUBJECT, "Disconnected from Greengrass Core with status: %s", status.StatusToString().c_str());
}

bool ConnectionEventsHandler::OnErrorCallback(RpcError status) {
    LOG_E(IPC_WRAPPER_SUBJECT, "Processing messages from the Greengrass Core resulted in error: %s",
          status.StatusToString().c_str());
    // TODO: Should we force close the connection here?
    return true;
}

GreengrassIPCWrapper::GreengrassIPCWrapper(GG::GreengrassCoreIpcClient *client)
    : crtApiHandle(new CRT::ApiHandle()), clientBootstrap(getClientBootstrap()),
      ipcClient(client != nullptr ? client : new GG::GreengrassCoreIpcClient(clientBootstrap)) {
    if (ipcClient == nullptr) {
        throw std::runtime_error("Failed to create IPC Client");
    }
};

// TODO: clean up apihandle without breaking tests
GreengrassIPCWrapper::~GreengrassIPCWrapper() { delete ipcClient; }

CRT::Io::ClientBootstrap &GreengrassIPCWrapper::getClientBootstrap() {
    CRT::Io::ClientBootstrap *clientBootstrap = CRT::ApiHandle::GetOrCreateStaticDefaultClientBootstrap();
    if (clientBootstrap == nullptr || clientBootstrap->LastError() != AWS_ERROR_SUCCESS) {
        if (clientBootstrap != nullptr) {
            LOG_E(IPC_WRAPPER_SUBJECT, "ClientBootstrap failed with error %s",
                  CRT::ErrorDebugString(clientBootstrap->LastError()));
        }
        throw std::runtime_error("Unable to get or create default client bootstrap");
    }
    return *clientBootstrap;
}

void GreengrassIPCWrapper::connect() {
    RpcError rpcError = ipcClient->Connect(connectionEventsHandler).get();
    if (!rpcError) {
        LOG_E(IPC_WRAPPER_SUBJECT, "Failed to establish connection IPC. Error %s", rpcError.StatusToString().c_str());
        throw std::runtime_error("Failed to connect to Greengrass Core");
    }
    LOG_I(IPC_WRAPPER_SUBJECT, "Greengrass IPC Client connected successfully!");
}

GG::GreengrassCoreIpcClient &GreengrassIPCWrapper::getIPCClient() { return *ipcClient; }

void GreengrassIPCWrapper::setAsRunning() {
    auto operation = ipcClient->NewUpdateState();
    GG::UpdateStateRequest request;
    request.SetState(Aws::Greengrass::REPORTED_LIFECYCLE_STATE_RUNNING);
    auto fut = operation->Activate(request, nullptr);
    fut.wait();
    operation->GetResult().wait_for(std::chrono::seconds(TIMEOUT_SECONDS));
    operation->Close();
}
