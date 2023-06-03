/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#pragma once

#include <aws/greengrass/GreengrassCoreIpcClient.h>
#include <filesystem>
#include <variant>

#include "logger.h"
#include "private/certificate_updater.h"
#include "private/configuration_subscriber.h"
#include "private/ipc_wrapper.h"

namespace GG = Aws::Greengrass;

// clang-format off
enum class AuthorizationStatus : int {
    AUTHORIZED = 0,
    UNAUTHORIZED = -1,
    BAD_AUTH_TOKEN = -2,
    UNKNOWN_ERROR = -3
};
// clang-format on

class ClientDeviceAuthIntegration {
  private:
    GreengrassIPCWrapper greengrassIpcWrapper;
    CertificateUpdater certificateUpdater;
    ConfigurationSubscriber configurationSubscriber;
    int timeoutSeconds;

    static const char *GET_CLIENT_DEVICE_AUTH_TOKEN_OP;
    static const char *AUTHORIZE_CLIENT_DEVICE_ACTION;
    static const char *VERIFY_CLIENT_DEVICE_IDENTITY;
    static const char *GET_CONFIGURATION_OP;

    static const char *INVALID_AUTH_TOKEN_ERROR;

  public:
    static const char *FAILED_OPERATION_FMT;
    static const char *FAILED_ACTIVATION_FMT;
    static const char *FAILED_TIMEOUT_ERROR_FMT;
    static const char *FAILED_RESPONSE_TYPE_FMT;
    static const char *FAILED_RESPONSE_MESSAGE_FMT;
    static const char *FAILED_RPC_ERROR_FMT;
    static const char *FAILED_NO_RESPONSE_VALUE;

    ClientDeviceAuthIntegration(GG::GreengrassCoreIpcClient *ipcClient, int timeoutSeconds)
        : greengrassIpcWrapper(ipcClient, timeoutSeconds), certificateUpdater(greengrassIpcWrapper.getIPCClient()),
          configurationSubscriber(greengrassIpcWrapper.getIPCClient()), timeoutSeconds(timeoutSeconds){};

    bool on_client_connect(const char *clientId, const char *pem);

    bool on_client_connected(const char *clientId, const char *pem);

    bool on_client_disconnected(const char *clientId, const char *pem);

    std::unique_ptr<std::string> get_client_device_auth_token(const char *clientId, const char *pem);

    AuthorizationStatus on_check_acl(const char *clientId, const char *authToken, const char *resource,
                                     const char *operation);

    bool verify_client_certificate(const char *certPem);

    void connect();

    GreengrassIPCWrapper &getIPCWrapper();

    ConfigurationSubscribeStatus subscribe_to_configuration_updates(std::unique_ptr<std::function<void()>> callback);

    std::variant<int, std::monostate, std::unique_ptr<Aws::Crt::JsonView>> get_configuration();

    CertSubscribeUpdateStatus
    subscribeToCertUpdates(std::unique_ptr<std::filesystem::path> basePath,
                           std::unique_ptr<std::function<void(GG::CertificateUpdateEvent *)>> subscription);

    static void handle_response_error(const char *action, const Aws::Eventstreamrpc::ResultType &responseType,
                                      Aws::Eventstreamrpc::OperationError *error);
};

ClientDeviceAuthIntegration *cda_integration_init(GG::GreengrassCoreIpcClient *client);

ClientDeviceAuthIntegration *cda_integration_init();

void cda_integration_close(ClientDeviceAuthIntegration *cda_integ);
