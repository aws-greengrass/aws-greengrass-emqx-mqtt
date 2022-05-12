/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#pragma once

#include <aws/greengrass/GreengrassCoreIpcClient.h>

namespace GG = Aws::Greengrass;

// TODO: create utils
static const std::string EMQX_DATA_ENV_VAR = "EMQX_NODE__DATA_DIR";
static const std::string EMQX_LOG_ENV_VAR = "EMQX_LOG__DIR";
static std::string getEnvVar(const std::string &envVarName) {
    char *value = getenv(envVarName.c_str());
    if (value == nullptr) {
        std::string err = "Failed to get env variable " + envVarName;
        LOG_E(CERT_UPDATER_SUBJECT, err.c_str());
        throw std::runtime_error(err.c_str());
    }
    return std::string(value);
}

class CertificateUpdatesHandler : public GG::SubscribeToCertificateUpdatesStreamHandler {
    void OnStreamEvent(GG::CertificateUpdateEvent *response) override;
    bool OnStreamError(OperationError *error) override;
    void OnStreamClosed() override;
    int writeCertsToFiles(Aws::Crt::String &privateKeyValue, Aws::Crt::String &certValue,
                          std::vector<Aws::Crt::String, Aws::Crt::StlAllocator<Aws::Crt::String>> &);
};

class CertificateUpdater {
  private:
    GG::GreengrassCoreIpcClient &ipcClient;
    std::shared_ptr<CertificateUpdatesHandler> updatesHandler;

  public:
    CertificateUpdater(GG::GreengrassCoreIpcClient &client)
        : ipcClient(client), updatesHandler(new CertificateUpdatesHandler()){};

    // TODO: Improve return codes
    int subscribeToUpdates();
};
