#include "../common.h"
#include "../defer.h"
#include "cda_integration.h"
#include <array>
#include <fstream>
#include <memory>

static const char *CRT_LOG_LEVEL_ENV_VAR = "CRT_LOG_LEVEL";
static const char *EMQX_LOG_LEVEL_ENV_VAR = "EMQX_LOG__LEVEL";
static const char *ORIGINAL_EMQX_DATA_DIR_ENV_VAR = "ORIG_EMQX_NODE__DATA_DIR";
static const char *ORIGINAL_EMQX_ETC_DIR_ENV_VAR = "ORIG_EMQX_NODE__ETC_DIR";
static const char *EMQX_DATA_DIR_ENV_VAR = "EMQX_NODE__DATA_DIR";
static const char *EMQX_ETC_DIR_ENV_VAR = "EMQX_NODE__ETC_DIR";

int copy_files() {
    // Copy all etc and data files from read-only into the writable location
    auto recursive_copy_options = std::filesystem::copy_options::recursive |
                                  std::filesystem::copy_options::overwrite_existing |
                                  std::filesystem::copy_options::skip_symlinks;

    const char *original_etc_dir = std::getenv(ORIGINAL_EMQX_ETC_DIR_ENV_VAR);
    if (original_etc_dir == nullptr || strlen(original_etc_dir) == 0) {
        LOG_E(CONFIG_WRITER_SUBJECT, "%s not set", ORIGINAL_EMQX_ETC_DIR_ENV_VAR);
        return 1;
    }
    const char *original_data_dir = std::getenv(ORIGINAL_EMQX_DATA_DIR_ENV_VAR);
    if (original_data_dir == nullptr || strlen(original_data_dir) == 0) {
        LOG_E(CONFIG_WRITER_SUBJECT, "%s not set", ORIGINAL_EMQX_DATA_DIR_ENV_VAR);
        return 1;
    }
    const char *new_etc_path = std::getenv(EMQX_ETC_DIR_ENV_VAR);
    if (new_etc_path == nullptr || strlen(new_etc_path) == 0) {
        LOG_E(CONFIG_WRITER_SUBJECT, "%s not set", EMQX_ETC_DIR_ENV_VAR);
        return 1;
    }
    const char *new_data_path = std::getenv(EMQX_DATA_DIR_ENV_VAR);
    if (new_data_path == nullptr || strlen(new_data_path) == 0) {
        LOG_E(CONFIG_WRITER_SUBJECT, "%s not set", EMQX_DATA_DIR_ENV_VAR);
        return 1;
    }

    // Copy from the original path to the new paths which we be loaded by EMQX
    std::filesystem::copy(original_etc_dir, new_etc_path, recursive_copy_options);
    std::filesystem::copy(original_data_dir, new_data_path, recursive_copy_options);
    return 0;
}

int main() {
    aws_log_level awsLogLevel;
    const char *crtLogLevel = std::getenv(CRT_LOG_LEVEL_ENV_VAR); // Log level may be null or empty
    if (crtLogLevel != nullptr && strlen(crtLogLevel) != 0) {
        // Use CRT_LOG_LEVEL when it is provided a non-empty
        awsLogLevel = crtStringToLogLevel(crtLogLevel);
    } else {
        // Otherwise, get the log level from the EMQX log level envvar
        const char *logLevel = std::getenv(EMQX_LOG_LEVEL_ENV_VAR); // Log level may be null or empty
        awsLogLevel = erlangStringToLogLevel(logLevel == nullptr ? "" : logLevel);
    }
    struct aws_logger_standard_options logger_options = {
        .level = awsLogLevel,
        .file = stderr,
    };
    // Use the noalloc logger which logs synchronously, otherwise some logs may not be emitted
    aws_logger_init_noalloc(&our_logger, aws_default_allocator(), &logger_options);
    aws_logger_set(&our_logger);

    if (copy_files() != 0) {
        return 1;
    }

    static const char *GetConfigurationRequest = "GetConfigurationRequest";
    auto cda_integration = std::unique_ptr<ClientDeviceAuthIntegration>(cda_integration_init());
    if (!cda_integration) {
        // No need to log. Failures are already logged in cda_integration_init()
        return 1;
    }
    GreengrassIPCWrapper &ipc = cda_integration->getIPCWrapper();
    try {
        ipc.connect();
    } catch (std::exception &e) {
        LOG_E(CONFIG_WRITER_SUBJECT, "failed to connect to AWS Greengrass due to %s", e.what());
        return 1;
    }

    auto &client = ipc.getIPCClient();
    auto operation = client.NewGetConfiguration();
    GG::GetConfigurationRequest request;
    // Read everything under the emqx namespace
    request.SetKeyPath({"emqx"});
    auto activate = operation->Activate(request).get();
    if (!activate) {
        LOG_E(CONFIG_WRITER_SUBJECT, ClientDeviceAuthIntegration::FAILED_ACTIVATION_FMT, GetConfigurationRequest,
              activate.StatusToString().c_str());
        return 1;
    }

    auto responseFuture = operation->GetOperationResult();
    if (responseFuture.wait_for(std::chrono::seconds(ipc.getTimeoutSeconds())) == std::future_status::timeout) {
        LOG_E(CONFIG_WRITER_SUBJECT, ClientDeviceAuthIntegration::FAILED_TIMEOUT_ERROR_FMT, GetConfigurationRequest);
        return 1;
    }
    auto responseResult = GG::GetConfigurationResult(responseFuture.get());
    auto responseType = responseResult.GetResultType();
    if (responseType != OPERATION_RESPONSE) {
        ClientDeviceAuthIntegration::handle_response_error(GetConfigurationRequest, responseType,
                                                           responseResult.GetOperationError());
        return 1;
    }

    // We now have the configuration!
    auto *response = responseResult.GetOperationResponse();
    if (response == nullptr || !response->GetValue().has_value()) {
        LOG_I(CONFIG_WRITER_SUBJECT, "Configuration response was empty");
        return 0;
    }

    auto configValue = response->GetValue().value();
    auto configView = configValue.View();

    if (configView.IsNull()) {
        LOG_I(CONFIG_WRITER_SUBJECT, "Configuration value was null");
        return 0;
    }
    if (!configView.IsObject()) {
        LOG_E(CONFIG_WRITER_SUBJECT, "Configuration /emqx was not an object as expected");
        return 1;
    }

    static const std::array<std::string, 33> allowed_files = {
        "data/loaded_plugins",
        "data/loaded_modules",
        "etc/emqx.conf",
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

    // Write customer-provided values to CWD
    const std::filesystem::path BASE_PATH = std::filesystem::current_path();
    for (const auto &item : configView.GetAllObjects()) {
        const auto key = item.first;
        const auto val = item.second;
        if (val.IsString()) {
            if (std::find(allowed_files.begin(), allowed_files.end(), key.c_str()) == allowed_files.end()) {
                LOG_I(CONFIG_WRITER_SUBJECT, "Ignoring unknown key %s", key.c_str());
                continue;
            }
            auto strVal = val.AsString();
            auto file_path = BASE_PATH / key.c_str();
            // try to create the directories as needed, ignoring errors
            std::filesystem::create_directories(file_path.parent_path());
            // Open file for appending
            auto out_path = std::ofstream(file_path);
            defer { out_path.close(); };
            out_path << strVal << std::endl;
        } else {
            LOG_I(CONFIG_WRITER_SUBJECT, "Value of %s was not a string", key.c_str());
        }
    }
}
