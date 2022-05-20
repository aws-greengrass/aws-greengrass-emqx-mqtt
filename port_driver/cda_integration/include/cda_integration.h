/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#pragma once

#include <aws/greengrass/GreengrassCoreIpcClient.h>
#include <filesystem>

#include "logger.h"
#include "private/certificate_updater.h"
#include "private/ipc_wrapper.h"

namespace GG = Aws::Greengrass;

class ClientDeviceAuthIntegration {
  private:
    GreengrassIPCWrapper greengrassIpcWrapper;
    CertificateUpdater certificateUpdater;

  public:
    ClientDeviceAuthIntegration(GG::GreengrassCoreIpcClient *ipcClient)
        : greengrassIpcWrapper(ipcClient), certificateUpdater(greengrassIpcWrapper.getIPCClient()){};

    bool on_client_connect(const char *clientId, const char *pem);

    bool on_client_connected(const char *clientId, const char *pem);

    bool on_client_disconnected(const char *clientId, const char *pem);

    std::unique_ptr<std::string> get_client_device_auth_token(const char *clientId, const char *pem);

    bool on_check_acl(const char *clientId, const char *authToken, const char *resource, const char *operation);

    bool verify_client_certificate(const char *certPem);

    void connect();

    CertSubscribeUpdateStatus
    subscribeToCertUpdates(std::unique_ptr<std::filesystem::path> basePath,
                           std::unique_ptr<std::function<void(GG::CertificateUpdateEvent *)>> subscription);
};

ClientDeviceAuthIntegration *cda_integration_init(GG::GreengrassCoreIpcClient *client);

ClientDeviceAuthIntegration *cda_integration_init();

void cda_integration_close(ClientDeviceAuthIntegration *cda_integ);
