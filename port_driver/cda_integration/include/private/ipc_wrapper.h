/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#pragma once

#include <aws/crt/Api.h>
#include <aws/greengrass/GreengrassCoreIpcClient.h>

namespace CRT = Aws::Crt;
namespace GG = Aws::Greengrass;

/*
 * Inheriting from ConnectionLifecycleHandler allows us to define callbacks that are
 * called upon when connection lifecycle events occur.
 */
class ConnectionEventsHandler : public ConnectionLifecycleHandler {
  public:
    ConnectionEventsHandler() = default;
    void OnConnectCallback() override;
    void OnDisconnectCallback(RpcError status) override;
    bool OnErrorCallback(RpcError status) override;
};

class GreengrassIPCWrapper {
  private:
    CRT::ApiHandle *crtApiHandle;
    CRT::Io::ClientBootstrap &clientBootstrap;
    GG::GreengrassCoreIpcClient *ipcClient;
    ConnectionEventsHandler connectionEventsHandler;
    int timeoutSeconds;

    static CRT::Io::ClientBootstrap &getClientBootstrap();

  public:
    GreengrassIPCWrapper(GG::GreengrassCoreIpcClient *client, int timeoutSeconds);
    void connect();
    ~GreengrassIPCWrapper();

    GG::GreengrassCoreIpcClient &getIPCClient();

    void setAsRunning();
};
