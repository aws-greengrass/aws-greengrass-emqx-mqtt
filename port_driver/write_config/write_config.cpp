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
static const char *EMQX_LOG_LEVEL_ENV_VAR = "EMQX_LOG__LEVEL";
static const char *ORIGINAL_EMQX_DATA_DIR_ENV_VAR = "ORIG_EMQX_NODE__DATA_DIR";
static const char *ORIGINAL_EMQX_ETC_DIR_ENV_VAR = "ORIG_EMQX_NODE__ETC_DIR";
static const char *EMQX_DATA_DIR_ENV_VAR = "EMQX_NODE__DATA_DIR";
static const char *EMQX_ETC_DIR_ENV_VAR = "EMQX_NODE__ETC_DIR";
static const char *GetConfigurationRequest = "GetConfigurationRequest";

static const char *RAW_CONFIG_NAMESPACE = "rawConfigurationFiles";
static const char *MERGE_CONFIG_NAMESPACE = "mergeConfigurationFiles";

static const char *EMQX_CONF_FILE = "etc/emqx.conf";
static const std::array<std::string, 33> allowed_files = {
    "data/loaded_plugins",
    "data/loaded_modules",
    EMQX_CONF_FILE,
    "etc/acl.conf",
    "etc/psk.txt",
    "etc/ssl_dist.conf",
    "etc/plugins/aws_greengrass_emqx_auth.conf",
    "etc/plugins/emqx_auth_http.conf",
    "etc/plugins/emqx_auth_jwt.conf",
    "etc/plugins/emqx_auth_ldap.conf",
    "etc/plugins/emqx_auth_mnesia.conf",
    "etc/plugins/emqx_auth_mongo.conf",
    "etc/plugins/emqx_auth_mysql.conf",
    "etc/plugins/emqx_auth_pgsql.conf",
    "etc/plugins/emqx_auth_redis.conf",
    "etc/plugins/emqx_bridge_mqtt.conf",
    "etc/plugins/emqx_coap.conf",
    "etc/plugins/emqx_dashboard.conf",
    "etc/plugins/emqx_exhook.conf",
    "etc/plugins/emqx_exproto.conf",
    "etc/plugins/emqx_lua_hook.conf",
    "etc/plugins/emqx_lwm2m.conf",
    "etc/plugins/emqx_management.conf",
    "etc/plugins/emqx_prometheus.conf",
    "etc/plugins/emqx_psk_file.conf",
    "etc/plugins/emqx_recon.conf",
    "etc/plugins/emqx_retainer.conf",
    "etc/plugins/emqx_rule_engine.conf",
    "etc/plugins/emqx_sasl.conf",
    "etc/plugins/emqx_sn.conf",
    "etc/plugins/emqx_stomp.conf",
    "etc/plugins/emqx_telemetry.conf",
    "etc/plugins/emqx_web_hook.conf",
};

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
        LOG_I(WRITE_CONFIG_SUBJECT, "Configuration value was not present");
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
        LOG_I(WRITE_CONFIG_SUBJECT, "Configuration response was empty");
        return 0;
    }

    return response->GetValue().value();
}

int read_config_and_update_files(GreengrassIPCWrapper &ipc, const char *config_namespace) {
    auto config_value = get_emqx_configuration(ipc, config_namespace);
    // If we couldn't get the configuration due to an error and need to exit, then exit with the
    // exit code from get_emqx_configuration.
    if (holds_alternative<int>(config_value)) {
        return get<int>(config_value);
    }
    auto config_view = get<Aws::Crt::JsonObject>(config_value).View();

    if (config_view.IsNull()) {
        LOG_I(WRITE_CONFIG_SUBJECT, "Configuration value /%s was null", config_namespace);
        return 0;
    }
    if (!config_view.IsObject()) {
        LOG_E(WRITE_CONFIG_SUBJECT, "Configuration /%s was present, but not an object", config_namespace);
        return 1;
    }

    const bool shouldAppend = config_namespace == MERGE_CONFIG_NAMESPACE;

    // Write customer-provided values to CWD
    const std::filesystem::path BASE_PATH = std::filesystem::current_path();
    // Configuration is in the form of
    // {"file/path": "file contents\n another line of file contents"}
    // For us to write the customer provided values into a config file, we must recognize the file path
    // from the JSON key as one of our supported paths in allowed_files. If the key is not in allowed_files
    // we will log that it is unknown, and then continue. This helps with security so that we are not writing
    // arbitrary files. If the user's provided value is not a string, then we will log that fact, but we will keep
    // going
    for (const auto &item : config_view.GetAllObjects()) {
        const auto possible_file_path = item.first;
        const auto customer_config_file_contents = item.second;
        if (customer_config_file_contents.IsString()) {
            if (std::find(allowed_files.begin(), allowed_files.end(), possible_file_path.c_str()) ==
                allowed_files.end()) {
                LOG_W(WRITE_CONFIG_SUBJECT, "Ignoring unknown key %s/%s", config_namespace, possible_file_path.c_str());
                continue;
            }

            // Raw config namespace does not allow etc/emqx.conf. Fail if it is provided here.
            if (!shouldAppend && possible_file_path == EMQX_CONF_FILE) {
                LOG_E(WRITE_CONFIG_SUBJECT, "%s is not allowed in %s", possible_file_path.c_str(), config_namespace);
                return 1;
            }

            auto strVal = customer_config_file_contents.AsString();
            auto file_path = BASE_PATH / possible_file_path.c_str();
            // try to create the directories as needed, ignoring errors
            std::filesystem::create_directories(file_path.parent_path());

            std::ofstream::openmode open_mode = std::ofstream::out;
            if (shouldAppend) {
                // enable append mode
                open_mode |= std::ofstream::app;
            }
            // Open file for writing. Will create file if it doesn't exist.
            auto out_path = std::ofstream(file_path, open_mode);
            defer { out_path.close(); };
            if (shouldAppend) {
                // Immediately add a new line so that user's contents definitely come in a line after the existing
                // values
                out_path << std::endl;
            }
            out_path << strVal << std::endl;
        } else {
            LOG_W(WRITE_CONFIG_SUBJECT, "Value of %s/%s was not a string", config_namespace,
                  possible_file_path.c_str());
        }
    }
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

    // Get user config for the raw and then merge. Merge will be applied on top of raw if
    // the same file is specified in both sections for some reason.
    for (const char *config_namespace : {RAW_CONFIG_NAMESPACE, MERGE_CONFIG_NAMESPACE}) {
        const int ret = read_config_and_update_files(ipc, config_namespace);
        if (ret != 0) {
            return ret;
        }
    }
    return 0;
}
