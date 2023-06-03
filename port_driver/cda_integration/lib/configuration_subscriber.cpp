/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include "private/configuration_subscriber.h"
#include "logger.h"
#include <aws/greengrass/GreengrassCoreIpcClient.h>

#define SUBSCRIBE_TIMEOUT_SECONDS 10

void ConfigurationUpdatesHandler::OnStreamEvent(
    GG::ConfigurationUpdateEvents *response) { // NOLINT(misc-unused-parameters)
    LOG_I(CONFIG_SUBSCRIBER_SUBJECT, "configurationUpdate stream event");
    callback->operator()();
}

bool ConfigurationUpdatesHandler::OnStreamError(OperationError *error) {
    if (error != nullptr) {
        LOG_E(CONFIG_SUBSCRIBER_SUBJECT, "configurationUpdate stream error %s", error->GetMessage().value().c_str());
    }
    return false; // keep the stream open
}

void ConfigurationUpdatesHandler::OnStreamClosed() {
    LOG_I(CONFIG_SUBSCRIBER_SUBJECT, "configurationUpdate stream closed");
}

ConfigurationSubscribeStatus
ConfigurationSubscriber::subscribe_to_configuration_updates(std::unique_ptr<std::string> componentName,
                                                            std::vector<std::string> keyPath,
                                                            std::unique_ptr<std::function<void()>> callback) {
    updatesHandler = std::make_shared<ConfigurationUpdatesHandler>(std::move(callback));
    operation = ipcClient.NewSubscribeToConfigurationUpdate(updatesHandler);
    if (!operation) {
        LOG_E(CONFIG_SUBSCRIBER_SUBJECT, "Unable to create SubscribeToConfigurationUpdateOperation.");
        return ConfigurationSubscribeStatus::SUBSCRIBE_ERROR_CREATE_OPERATION;
    }

    GG::SubscribeToConfigurationUpdateRequest request;
    if (componentName) {
        request.SetComponentName(Aws::Crt::String(componentName->c_str())); // NOLINT(readability-redundant-string-cstr)
    }
    request.SetKeyPath(Aws::Crt::Vector<Aws::Crt::String>(keyPath.begin(), keyPath.end()));

    auto activate = operation->Activate(request, nullptr);
    activate.wait();

    auto responseFuture = operation->GetOperationResult();
    if (responseFuture.wait_for(std::chrono::seconds(SUBSCRIBE_TIMEOUT_SECONDS)) == std::future_status::timeout) {
        LOG_E(CONFIG_SUBSCRIBER_SUBJECT, "Operation timed out while waiting for response from Greengrass Core.");
        return ConfigurationSubscribeStatus::SUBSCRIBE_ERROR_TIMEOUT_RESPONSE;
    }

    auto response = GG::SubscribeToConfigurationUpdateResult(responseFuture.get());
    if (!response) {
        auto responseType = response.GetResultType();
        LOG_E(CONFIG_SUBSCRIBER_SUBJECT, "Subscribe failed with response type %d", responseType);
        if (responseType == OPERATION_ERROR) {
            auto *error = response.GetOperationError();
            if (error != nullptr) {
                LOG_E(CONFIG_SUBSCRIBER_SUBJECT, "Config updates subscribe operation failure response: %s",
                      error->GetMessage().value().c_str());
            }
        } else if (responseType == RPC_ERROR) {
            auto error = response.GetRpcError();
            LOG_E(CONFIG_SUBSCRIBER_SUBJECT, "Config updates RPC failure response: %s", error.StatusToString().c_str())
        }
        return ConfigurationSubscribeStatus::SUBSCRIBE_ERROR_FAILURE_RESPONSE;
    }

    LOG_I(CONFIG_SUBSCRIBER_SUBJECT, "Successfully subscribed to config updates");
    return ConfigurationSubscribeStatus::SUBSCRIBE_SUCCESS;
}
