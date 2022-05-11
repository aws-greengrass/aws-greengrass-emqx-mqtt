/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#pragma once

#include <aws/greengrass/GreengrassCoreIpcClient.h>

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

    void close() const;

    bool on_client_connect(const char *clientId, const char *pem);

    bool on_client_connected(const char *clientId, const char *pem);

    bool on_client_disconnected(const char *clientId, const char *pem);

    bool on_client_authenticate(const char *clientId, const char *pem);

    bool on_check_acl(const char *clientId, const char *pem, const char *topic, const char *action);

    bool verify_client_certificate(const char *certPem);

    int subscribeToCertUpdates();
};

ClientDeviceAuthIntegration *cda_integration_init(GG::GreengrassCoreIpcClient *client);

ClientDeviceAuthIntegration *cda_integration_init();

void cda_integration_close(ClientDeviceAuthIntegration *cda_integ);
