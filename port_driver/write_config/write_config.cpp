/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include "../common.h"
#include "../defer.h"
#include "cda_integration.h"
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
static const char *EMQX_CONF_FILE = "emqx.conf";
static const char *KEY_EMQX_CONFIG = "emqxConfig";

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

std::variant<int, Aws::Crt::JsonObject> get_emqx_configuration(GreengrassIPCWrapper &ipc) {
    auto &client = ipc.getIPCClient();
    auto operation = client.NewGetConfiguration();
    GG::GetConfigurationRequest request;
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
        LOG_I(WRITE_CONFIG_SUBJECT, "Component configuration is not present. Configuration changes will not apply");
        return 0;
    }
    if (responseType != OPERATION_RESPONSE) {
        ClientDeviceAuthIntegration::handle_response_error(GetConfigurationRequest, responseType,
                                                           responseResult.GetOperationError());
        return 1;
    }

    auto *response = responseResult.GetOperationResponse();
    if (response == nullptr || !response->GetValue().has_value()) {
        LOG_I(WRITE_CONFIG_SUBJECT, "Component configuration missing. No configuration changes will apply");
        return 0;
    }

    return response->GetValue().value();
}

int read_config_and_update_files(GreengrassIPCWrapper &ipc) {
    auto config_value = get_emqx_configuration(ipc);
    // If we couldn't get the configuration due to an error and need to exit, then exit with the
    // exit code from get_emqx_configuration.
    if (holds_alternative<int>(config_value)) {
        return get<int>(config_value);
    }
    auto config_view = get<Aws::Crt::JsonObject>(config_value).View();
    if (config_view.IsNull()) {
        LOG_I(WRITE_CONFIG_SUBJECT, "Component configuration missing. No configuration changes will apply");
        return 0;
    }
    if (!config_view.IsObject()) {
        LOG_E(WRITE_CONFIG_SUBJECT, "Component configuration is not an object.");
        return 1;
    }

    // Write customer-provided values to CWD
    const std::filesystem::path etc_dir(std::getenv(EMQX_ETC_DIR_ENV_VAR));
    auto emqx_conf = etc_dir / EMQX_CONF_FILE;

    // Try to create the directories as needed, ignoring errors
    std::filesystem::create_directories(emqx_conf.parent_path());

    // Configuration is in the form of
    // {"emqxConfig": {"listeners": {"ssl": {"default": ...}}}}
    // We do not check if the config is valid, this is handled by EMQx.
    auto emqx_config = config_view.AsObject().GetJsonObject(KEY_EMQX_CONFIG);
    if (emqx_config.IsNull() || emqx_config.GetAllObjects().empty()) {
        LOG_I(WRITE_CONFIG_SUBJECT, "Configuration /%s not present. Configuration from /%s will not be applied.",
              KEY_EMQX_CONFIG, KEY_EMQX_CONFIG);
        return 0;
    }

    if (!emqx_config.IsObject()) {
        LOG_I(WRITE_CONFIG_SUBJECT,
              "Configuration /%s is present, but not an object. Configuration from /%s will not be applied. Fix this "
              "by updating the deployment with \"RESET\"[\"%s\"]",
              KEY_EMQX_CONFIG, KEY_EMQX_CONFIG, KEY_EMQX_CONFIG);
        return 1;
    }

    if (emqx_config.AsObject().GetAllObjects().empty()) {
        LOG_I(WRITE_CONFIG_SUBJECT, "Configuration /%s is empty. Configuration from /%s will not be applied.",
              KEY_EMQX_CONFIG, KEY_EMQX_CONFIG);
        return 0;
    }

    Aws::Crt::String output = emqx_config.AsObject().WriteReadable();

    // trimming open and close braces before appending to emqx config.
    // as seen during our testing, HOCON won't support multiple JSON docs in a single file.
    auto opening_brace = output.find_first_of('{');
    if (opening_brace != std::string::npos) {
        output.erase(opening_brace);
    }
    auto closing_brace = output.find_last_of('}');
    if (closing_brace != std::string::npos) {
        output.erase(closing_brace);
    }

    LOG_I(WRITE_CONFIG_SUBJECT, "Writing configuration to %s", emqx_conf.string().c_str());

    // Open file for writing. Will create file if it doesn't exist.
    auto out_path = std::ofstream(emqx_conf, std::ofstream::app);
    defer { out_path.close(); };
    out_path << output << std::endl;

    LOG_I(WRITE_CONFIG_SUBJECT, "Appended %s with customer config", EMQX_CONF_FILE);

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

    // Get user config for the override file. The config will be appended to EMQx's
    // override file. This can be updated to include cluster-override
    // in the future.
    const int ret = read_config_and_update_files(ipc);
    if (ret != 0) {
        return ret;
    }
    return 0;
}
