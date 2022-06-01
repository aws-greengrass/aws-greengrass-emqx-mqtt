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
    static const char *FAILED_OPERATION_FMT;
    static const char *FAILED_ACTIVATION_FMT;
    static const char *FAILED_TIMEOUT_ERROR_FMT;
    static const char *FAILED_RESPONSE_TYPE_FMT;
    static const char *FAILED_RESPONSE_MESSAGE_FMT;
    static const char *FAILED_RPC_ERROR_FMT;
    static const char *FAILED_NO_RESPONSE_VALUE;

    static const char *GET_CLIENT_DEVICE_AUTH_TOKEN_OP;
    static const char *AUTHORIZE_CLIENT_DEVICE_ACTION;
    static const char *VERIFY_CLIENT_DEVICE_IDENTITY;

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

    static void handle_response_error(const char *action, const Aws::Eventstreamrpc::ResultType &responseType,
                                      Aws::Eventstreamrpc::OperationError *error);
};

ClientDeviceAuthIntegration *cda_integration_init(GG::GreengrassCoreIpcClient *client);

ClientDeviceAuthIntegration *cda_integration_init();

void cda_integration_close(ClientDeviceAuthIntegration *cda_integ);
