/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include "../common.h"
#include "../defer.h"
#include "cda_integration.h"
#include "config.h"
#include <array>
#include <fstream>
#include <memory>
#include <variant>

static const char *CRT_LOG_LEVEL_ENV_VAR = "CRT_LOG_LEVEL";
static const char *EMQX_LOG_LEVEL_ENV_VAR = "EMQX_LOG__CONSOLE_HANDLER__LEVEL";
static const char *ORIGINAL_EMQX_DATA_DIR_ENV_VAR = "ORIG_EMQX_NODE__DATA_DIR";
static const char *ORIGINAL_EMQX_ETC_DIR_ENV_VAR = "ORIG_EMQX_NODE__ETC_DIR";
static const char *EMQX_DATA_DIR_ENV_VAR = "EMQX_NODE__DATA_DIR";
static const char *EMQX_ETC_DIR_ENV_VAR = "EMQX_NODE__ETC_DIR";
static const char *GetConfigurationRequest = "GetConfigurationRequest";
static const char *LOCAL_CONF_FILE = "configs/local-override.conf";

static struct aws_logger our_logger {};

int copy_default_config() {
    // Copy all etc and data files from read-only into the writable location
    auto recursive_copy_options = std::filesystem::copy_options::recursive |
                                  std::filesystem::copy_options::overwrite_existing |
                                  std::filesystem::copy_options::skip_symlinks;

    const char *original_etc_dir = std::getenv(ORIGINAL_EMQX_ETC_DIR_ENV_VAR);
    if (original_etc_dir == nullptr || strlen(original_etc_dir) == 0) {
        LOG_E(WRITE_CONFIG_SUBJECT, "%s not set", ORIGINAL_EMQX_ETC_DIR_ENV_VAR);
        return 1;
    }
    const char *original_data_dir = std::getenv(ORIGINAL_EMQX_DATA_DIR_ENV_VAR);
    if (original_data_dir == nullptr || strlen(original_data_dir) == 0) {
        LOG_E(WRITE_CONFIG_SUBJECT, "%s not set", ORIGINAL_EMQX_DATA_DIR_ENV_VAR);
        return 1;
    }
    const char *new_etc_path = std::getenv(EMQX_ETC_DIR_ENV_VAR);
    if (new_etc_path == nullptr || strlen(new_etc_path) == 0) {
        LOG_E(WRITE_CONFIG_SUBJECT, "%s not set", EMQX_ETC_DIR_ENV_VAR);
        return 1;
    }
    const char *new_data_path = std::getenv(EMQX_DATA_DIR_ENV_VAR);
    if (new_data_path == nullptr || strlen(new_data_path) == 0) {
        LOG_E(WRITE_CONFIG_SUBJECT, "%s not set", EMQX_DATA_DIR_ENV_VAR);
        return 1;
    }

    LOG_I(WRITE_CONFIG_SUBJECT, "Copying default EMQX configuration");

    // Copy from the original path to the new paths which we be loaded by EMQX
    std::filesystem::copy(original_etc_dir, new_etc_path, recursive_copy_options);
    std::filesystem::copy(original_data_dir, new_data_path, recursive_copy_options);
    return 0;
}

void setup_logger() {
    aws_log_level awsLogLevel;
    const char *crtLogLevel = getenv(CRT_LOG_LEVEL_ENV_VAR); // Log level may be null or empty
    if (crtLogLevel != nullptr && strlen(crtLogLevel) != 0) {
        // Use CRT_LOG_LEVEL when it is provided a non-empty
        awsLogLevel = crtStringToLogLevel(crtLogLevel);
    } else {
        // Otherwise, get the log level from the EMQX log level envvar
        const char *logLevel = getenv(EMQX_LOG_LEVEL_ENV_VAR); // Log level may be null or empty
        awsLogLevel = erlangStringToLogLevel(logLevel == nullptr ? "" : logLevel);
    }
    struct aws_logger_standard_options logger_options = {
        .level = awsLogLevel,
        .file = stderr,
    };
    // Use the noalloc logger which logs synchronously, otherwise some logs may not be emitted
    aws_logger_init_noalloc(&our_logger, aws_default_allocator(), &logger_options);
    aws_logger_set(&our_logger);
}

std::variant<int, Aws::Crt::JsonObject> get_emqx_configuration(GreengrassIPCWrapper &ipc, const char *config_key) {
    auto &client = ipc.getIPCClient();
    auto operation = client.NewGetConfiguration();
    GG::GetConfigurationRequest request;
    request.SetKeyPath({config_key});
    auto activate = operation->Activate(request).get();
    if (!activate) {
        LOG_E(WRITE_CONFIG_SUBJECT, ClientDeviceAuthIntegration::FAILED_ACTIVATION_FMT, GetConfigurationRequest,
              activate.StatusToString().c_str());
        return 1;
    }

    auto responseFuture = operation->GetOperationResult();
    if (responseFuture.wait_for(std::chrono::seconds(ipc.getTimeoutSeconds())) == std::future_status::timeout) {
        LOG_E(WRITE_CONFIG_SUBJECT, ClientDeviceAuthIntegration::FAILED_TIMEOUT_ERROR_FMT, GetConfigurationRequest);
        return 1;
    }
    auto responseResult = GG::GetConfigurationResult(responseFuture.get());
    auto responseType = responseResult.GetResultType();
    if (responseResult.GetOperationError() != nullptr &&
        responseResult.GetOperationError()->GetModelName() == GG::ResourceNotFoundError::MODEL_NAME) {
        LOG_I(
            WRITE_CONFIG_SUBJECT,
            "Configuration /%s was not present. This is not a problem, but no configuration changes in /%s will apply",
            config_key, config_key);
        return 0;
    }
    if (responseType != OPERATION_RESPONSE) {
        ClientDeviceAuthIntegration::handle_response_error(GetConfigurationRequest, responseType,
                                                           responseResult.GetOperationError());
        return 1;
    }

    // We now have the configuration
    auto *response = responseResult.GetOperationResponse();
    if (response == nullptr || !response->GetValue().has_value()) {
        LOG_I(WRITE_CONFIG_SUBJECT,
              "Configuration /%s was empty. This is not a problem, but no configuration changes in /%s will apply",
              config_key, config_key);
        return 0;
    }

    return response->GetValue().value();
}

int read_config_and_update_files(GreengrassIPCWrapper &ipc) {
    auto config_value = get_emqx_configuration(ipc, aws::greengrass::emqx::localOverrideNamespace.c_str());
    // If we couldn't get the configuration due to an error and need to exit, then exit with the
    // exit code from get_emqx_configuration.
    if (holds_alternative<int>(config_value)) {
        return get<int>(config_value);
    }
    auto config_view = get<Aws::Crt::JsonObject>(config_value).View();

    if (config_view.IsNull()) {
        LOG_I(WRITE_CONFIG_SUBJECT,
              "Configuration value /%s was null. This is not a problem, but no configuration changes in /%s will apply",
              aws::greengrass::emqx::localOverrideNamespace.c_str(),
              aws::greengrass::emqx::localOverrideNamespace.c_str());
        return 0;
    }
    if (!config_view.IsObject()) {
        LOG_E(WRITE_CONFIG_SUBJECT,
              "Configuration /%s was present, but not an object. Configuration from /%s will not be applied. Fix this "
              "by updating the deployment with "
              "\"RESET\":[\"/%s\"]",
              aws::greengrass::emqx::localOverrideNamespace.c_str(),
              aws::greengrass::emqx::localOverrideNamespace.c_str(),
              aws::greengrass::emqx::localOverrideNamespace.c_str());
        return 1;
    }

    // Write customer-provided values to CWD
    const std::filesystem::path data_dir(std::getenv(EMQX_DATA_DIR_ENV_VAR));
    auto file_path = data_dir / LOCAL_CONF_FILE;

    // Try to create the directories as needed, ignoring errors
    std::filesystem::create_directories(file_path.parent_path());

    // Open file for writing. Will create file if it doesn't exist.
    std::ofstream::openmode open_mode = std::ofstream::out;
    auto out_path = std::ofstream(file_path, open_mode);
    defer { out_path.close(); };

    // Configuration is in the form of
    // {"localOverride": {"listeners": {"ssl": {"default": ...}}}}
    // We are looking up the "localOverride" key so we receive a JSON of the config starting from "{listeners: ...}"
    // We then replace the contents of local-override.conf with the user provided config. We do not check if the
    // config is valid, this is handled by EMQx.
    Aws::Crt::String output = config_view.WriteReadable();
    LOG_I(WRITE_CONFIG_SUBJECT, "Replacing %s with customer provided override", LOCAL_CONF_FILE);
    out_path << output << std::endl;

    return 0;
}

int main() {
    setup_logger();

    if (copy_default_config() != 0) {
        return 1;
    }

    auto cda_integration = std::unique_ptr<ClientDeviceAuthIntegration>(cda_integration_init());
    if (!cda_integration) {
        // No need to log. Failures are already logged in cda_integration_init()
        return 1;
    }
    GreengrassIPCWrapper &ipc = cda_integration->getIPCWrapper();
    try {
        ipc.connect();
    } catch (std::exception &e) {
        LOG_E(WRITE_CONFIG_SUBJECT, "failed to connect to AWS Greengrass due to %s", e.what());
        return 1;
    }

    // Get user config for local-override. The config will be appended to EMQx's
    // local-override.conf. This can be updated to include cluster-override
    // in the future.
    const int ret = read_config_and_update_files(ipc);
    if (ret != 0) {
        return ret;
    }
    return 0;
}
