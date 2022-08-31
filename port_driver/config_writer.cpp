#include "common.h"
#include "defer.h"
#include <array>
#include <cda_integration.h>
#include <fstream>
#include <memory>

static const char *CRT_LOG_LEVEL_ENV_VAR = "CRT_LOG_LEVEL";
static const char *EMQX_LOG_LEVEL_ENV_VAR = "EMQX_LOG__LEVEL";

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

    static const std::array<std::string, 36> allowed_files = {
        "emqx.conf",
        "acl.conf",
        "psk.txt",
        "ssl_dist.conf",
        "plugins/acl.conf.paho",
        "plugins/aws_greengrass_emqx_auth.conf",
        "plugins/emqx_auth_http.conf",
        "plugins/emqx_auth_jwt.conf",
        "plugins/emqx_auth_ldap.conf",
        "plugins/emqx_auth_mnesia.conf",
        "plugins/emqx_auth_mongo.conf",
        "plugins/emqx_auth_mysql.conf",
        "plugins/emqx_auth_pgsql.conf",
        "plugins/emqx_auth_redis.conf",
        "plugins/emqx_bridge_mqtt.conf",
        "plugins/emqx_coap.conf",
        "plugins/emqx_dashboard.conf",
        "plugins/emqx_exhook.conf",
        "plugins/emqx_exproto.conf",
        "plugins/emqx_lua_hook.conf",
        "plugins/emqx_lwm2m.conf",
        "plugins/emqx_management.conf",
        "plugins/emqx_prometheus.conf",
        "plugins/emqx_psk_file.conf",
        "plugins/emqx_recon.conf",
        "plugins/emqx_retainer.conf",
        "plugins/emqx_rule_engine.conf",
        "plugins/emqx_sasl.conf",
        "plugins/emqx_sn.conf",
        "plugins/emqx_stomp.conf",
        "plugins/emqx_telemetry.conf",
        "plugins/emqx_web_hook.conf",
    };

    // Write customer-provided values to CWD/etc
    const std::filesystem::path ETC_BASE_PATH = "etc";

    for (const auto &item : configView.GetAllObjects()) {
        const auto key = item.first;
        const auto val = item.second;
        if (val.IsString()) {
            if (std::find(allowed_files.begin(), allowed_files.end(), key.c_str()) == allowed_files.end()) {
                LOG_I(CONFIG_WRITER_SUBJECT, "Ignoring unknown key %s", key.c_str());
                continue;
            }
            auto strVal = val.AsString();
            if (!strVal.empty()) {
                auto file_path = ETC_BASE_PATH / key.c_str();
                // try to create the directories as needed, ignoring errors
                std::filesystem::create_directories(file_path.parent_path());
                auto out_path = std::ofstream(file_path);
                defer { out_path.close(); };
                out_path << strVal << std::endl;
            }
        } else {
            LOG_I(CONFIG_WRITER_SUBJECT, "Value of %s was not a string", key.c_str());
        }
    }
}
