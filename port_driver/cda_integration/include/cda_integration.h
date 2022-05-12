/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#pragma once

#include <aws/greengrass/GreengrassCoreIpcClient.h>

#include "private/certificate_updater.h"
#include "private/ipc_wrapper.h"
#include "logger.h"

namespace GG = Aws::Greengrass;

//TODO: create utils
static const std::string EMQX_DATA_ENV_VAR = "EMQX_NODE__DATA_DIR";
static const std::string EMQX_LOG_ENV_VAR = "EMQX_LOG__DIR";
static std::string getEnvVar(const std::string &envVarName) {
    char* value = getenv(envVarName.c_str());
    if (value == nullptr) {
        std::string err = "Failed to get env variable " + envVarName;
        LOG_E(CERT_UPDATER_SUBJECT, err.c_str());
        throw std::runtime_error(err.c_str());
    }
    return std::string(value);
}

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

    bool on_client_authenticate(const char *clientId, const char *pem);

    bool on_check_acl(const char *clientId, const char *pem, const char *topic, const char *action);

    bool verify_client_certificate(const char *certPem);

    void connect();

    void subscribeToCertUpdates();
};

ClientDeviceAuthIntegration *cda_integration_init(GG::GreengrassCoreIpcClient *client);

ClientDeviceAuthIntegration *cda_integration_init();

void cda_integration_close(ClientDeviceAuthIntegration *cda_integ);
