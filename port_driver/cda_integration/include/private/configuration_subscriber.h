/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#pragma once

#include <aws/greengrass/GreengrassCoreIpcClient.h>

namespace GG = Aws::Greengrass;

enum class ConfigurationSubscribeStatus : int {
    SUBSCRIBE_SUCCESS = 0,
    SUBSCRIBE_ERROR_CREATE_OPERATION = -1,
    SUBSCRIBE_ERROR_TIMEOUT_RESPONSE = -2,
    SUBSCRIBE_ERROR_FAILURE_RESPONSE = -3
};

class ConfigurationUpdatesHandler : public GG::SubscribeToConfigurationUpdateStreamHandler {
  public:
    bool OnStreamError(OperationError *error) override;
    void OnStreamClosed() override;
    void OnStreamEvent(GG::ConfigurationUpdateEvents *response) override;
    virtual ~ConfigurationUpdatesHandler() = default;

    ConfigurationUpdatesHandler(std::unique_ptr<std::function<void()>> callback) : callback(std::move(callback)) {}

  private:
    const std::unique_ptr<std::function<void()>> callback;
};

class ConfigurationSubscriber {
  public:
    inline static const std::string localOverrideNamespace = "localOverride";

    ConfigurationSubscriber(GG::GreengrassCoreIpcClient &client) : ipcClient(client), updatesHandler({}){};
    ConfigurationSubscribeStatus subscribe_to_configuration_updates(std::unique_ptr<std::function<void()>> callback);

  private:
    GG::GreengrassCoreIpcClient &ipcClient;
    std::shared_ptr<ConfigurationUpdatesHandler> updatesHandler;
    std::shared_ptr<GG::SubscribeToConfigurationUpdateOperation> operation;
};
