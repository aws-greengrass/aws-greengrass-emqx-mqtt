/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include "port_driver.h"
#include "defer.h"
#include <aws/crt/Api.h>
#include <cda_integration.h>
#include <cstring>
#include <filesystem>
#include <logger.h>
#include <memory>
#include <string>

using DriverContext = struct {
    ErlDrvPort port;
    ClientDeviceAuthIntegration *cda_integration;
};

static struct aws_logger our_logger {};

struct atoms {
    ErlDrvTermData data;
    ErlDrvTermData pass;
    ErlDrvTermData fail;
    ErlDrvTermData valid;
    ErlDrvTermData invalid;
    ErlDrvTermData authorized;
    ErlDrvTermData unauthorized;
    ErlDrvTermData unknown;
    ErlDrvTermData event;
    ErlDrvTermData certificate_update;
};
static struct atoms ATOMS {};
static const char *CONSOLE = "console";
static const char *EMQX_LOG_TO_ENV_VAR = "EMQX_LOG__TO";
static const char *EMQX_LOG_LEVEL_ENV_VAR = "EMQX_LOG__LEVEL";
static const char *EMQX_LOG_ENV_VAR = "EMQX_LOG__DIR";
static const char *EMQX_DATA_ENV_VAR = "EMQX_NODE__DATA_DIR";

static aws_log_level stringToLogLevel(const std::string &level) {
    // Erlang log levels
    // debug, info, notice, warning, error, critical, alert, emergency
    if (level == "debug") {
        return AWS_LL_TRACE;
    }
    if (level == "info") {
        return AWS_LL_DEBUG;
    }
    if (level == "notice") {
        return AWS_LL_INFO;
    }
    if (level == "warning") {
        return AWS_LL_WARN;
    }
    if (level == "error") {
        return AWS_LL_ERROR;
    }
    if (level == "emergency") {
        return AWS_LL_FATAL;
    }
    // Default to warn
    return AWS_LL_WARN;
}

EXPORTED ErlDrvData drv_start(ErlDrvPort port, char *buff) { // NOLINT(readability-non-const-parameter)
    (void)buff;
    const char *logLocation = std::getenv(EMQX_LOG_TO_ENV_VAR);
    if (logLocation == nullptr) {
        // NOLINTNEXTLINE(cppcoreguidelines-pro-type-cstyle-cast, performance-no-int-to-ptr)
        return ERL_DRV_ERROR_BADARG;
    }
    const char *logLevel = std::getenv(EMQX_LOG_LEVEL_ENV_VAR); // Log level may be null
    aws_log_level awsLogLevel = stringToLogLevel(logLevel == nullptr ? "" : logLevel);
    if (std::string_view{CONSOLE} == logLocation) {
        struct aws_logger_standard_options logger_options = {
            .level = awsLogLevel,
            .file = stderr,
        };
        aws_logger_init_standard(&our_logger, aws_default_allocator(), &logger_options);
    } else {
        const char *logDir = std::getenv(EMQX_LOG_ENV_VAR);
        if (logDir == nullptr) {
            // NOLINTNEXTLINE(cppcoreguidelines-pro-type-cstyle-cast, performance-no-int-to-ptr)
            return ERL_DRV_ERROR_BADARG;
        }
        // try to create the log directories as needed, ignoring errors
        std::filesystem::create_directories(logDir);
        const std::filesystem::path logPath = std::filesystem::path(logDir).append("crt.log");
        const auto *logPathStr = new std::string{logPath.string()};
        struct aws_logger_standard_options logger_options = {
            .level = awsLogLevel,
            .filename = logPathStr->c_str(),
        };
        aws_logger_init_standard(&our_logger, aws_default_allocator(), &logger_options);
    }
    aws_logger_set(&our_logger);

    LOG_I(PORT_DRIVER_SUBJECT, "Starting AWS Greengrass auth driver");
    ei_init();

    // Setup static atoms
    ATOMS.data = driver_mk_atom(const_cast<char *>("data"));
    ATOMS.pass = driver_mk_atom(const_cast<char *>("pass"));
    ATOMS.fail = driver_mk_atom(const_cast<char *>("fail"));
    ATOMS.valid = driver_mk_atom(const_cast<char *>("valid"));
    ATOMS.invalid = driver_mk_atom(const_cast<char *>("invalid"));
    ATOMS.authorized = driver_mk_atom(const_cast<char *>("authorized"));
    ATOMS.unauthorized = driver_mk_atom(const_cast<char *>("unauthorized"));
    ATOMS.unknown = driver_mk_atom(const_cast<char *>("unknown"));
    ATOMS.certificate_update = driver_mk_atom(const_cast<char *>("certificate_update"));
    ATOMS.event = driver_mk_atom(const_cast<char *>("event"));

    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    auto *context = reinterpret_cast<DriverContext *>(driver_alloc(sizeof(DriverContext)));
    context->port = port;
    context->cda_integration = cda_integration_init();
    if (context->cda_integration == nullptr) {
        LOG_E(PORT_DRIVER_SUBJECT, "Failed to initialize CDA integration");

        // NOLINTNEXTLINE(cppcoreguidelines-pro-type-cstyle-cast, performance-no-int-to-ptr)
        return ERL_DRV_ERROR_GENERAL;
    }
    // TODO: move this to cda_integration_init() once tests are changed
    try {
        context->cda_integration->connect();
    } catch (std::exception &e) {
        LOG_E(PORT_DRIVER_SUBJECT, "failed to connect to AWS Greengrass due to %s", e.what());
        // NOLINTNEXTLINE(cppcoreguidelines-pro-type-cstyle-cast, performance-no-int-to-ptr)
        return ERL_DRV_ERROR_GENERAL;
    }
    return reinterpret_cast<ErlDrvData>(context);
}

EXPORTED void drv_stop(ErlDrvData handle) {
    LOG_I(PORT_DRIVER_SUBJECT, "Stopping AWS Greengrass auth driver");
    auto *context = reinterpret_cast<DriverContext *>(handle);
    cda_integration_close(context->cda_integration);
    driver_free(reinterpret_cast<void *>(handle));
    aws_logger_clean_up(&our_logger);
}

static void send_event_to_port(DriverContext *context, ErlDrvTermData eventAtom) {
    auto port = driver_mk_port(context->port);

    // https://www.erlang.org/doc/man/erl_driver.html#erl_drv_output_term
    // The follow code encodes this Erlang term: {Port, event, event atom}

    ErlDrvTermData spec[] = {ERL_DRV_PORT, port, ERL_DRV_ATOM, ATOMS.event, ERL_DRV_ATOM, eventAtom, ERL_DRV_TUPLE, 3};
    if (erl_drv_output_term(port, spec, sizeof(spec) / sizeof(spec[0])) < 0) {
        LOG_E(PORT_DRIVER_SUBJECT, "Failed outputting atom event");
    }
}

static void write_atom_to_port(DriverContext *context, ErlDrvTermData result, const char return_code) {
    auto port = driver_mk_port(context->port);

    // https://www.erlang.org/doc/man/erl_driver.html#erl_drv_output_term
    // The follow code encodes this Erlang term: {Port, {data, [return code integer, result atom]}}

    ErlDrvTermData spec[] = {
        ERL_DRV_PORT,  port,   ERL_DRV_ATOM, ATOMS.data, ERL_DRV_INT,   (ErlDrvTermData)return_code,
        ERL_DRV_ATOM,  result, ERL_DRV_LIST, 2,          ERL_DRV_TUPLE, 2,
        ERL_DRV_TUPLE, 2};
    if (erl_drv_output_term(port, spec, sizeof(spec) / sizeof(spec[0])) < 0) {
        LOG_E(PORT_DRIVER_SUBJECT, "Failed outputting atom result");
    }
}

static void write_string_to_port(DriverContext *context, const std::string &result, const char return_code) {
    auto port = driver_mk_port(context->port);

    // https://www.erlang.org/doc/man/erl_driver.html#erl_drv_output_term
    // The follow code encodes this Erlang term: {Port, {data, [return code integer, result string]}}
    ErlDrvTermData spec[] = {ERL_DRV_PORT,
                             port,
                             ERL_DRV_ATOM,
                             ATOMS.data,
                             ERL_DRV_INT,
                             (ErlDrvTermData)return_code,
                             ERL_DRV_STRING,
                             reinterpret_cast<ErlDrvTermData>(result.c_str()),
                             result.length(),
                             ERL_DRV_LIST,
                             2,
                             ERL_DRV_TUPLE,
                             2,
                             ERL_DRV_TUPLE,
                             2};

    if (erl_drv_output_term(port, spec, sizeof(spec) / sizeof(spec[0])) < 0) {
        LOG_E(PORT_DRIVER_SUBJECT, "Failed outputting string result");
    }
}

static void write_async_bool_to_port(DriverContext *context, EI_LONGLONG requestId, ErlDrvTermData result,
                                     const char returnCode) {
    auto port = driver_mk_port(context->port);

    // https://www.erlang.org/doc/man/erl_driver.html#erl_drv_output_term
    // The follow code encodes this Erlang term: {Port, request id integer, {data, [return code integer, true or false
    // atom]}}

    // Request ID in this case is a pointer to stack memory, but that's fine because erl_drv_output_term copies
    // the data immediately into the Erlang heap.
    ErlDrvTermData spec[] = {ERL_DRV_PORT,  port,       ERL_DRV_INT64, reinterpret_cast<ErlDrvTermData>(&requestId),
                             ERL_DRV_ATOM,  ATOMS.data, ERL_DRV_INT,   static_cast<ErlDrvTermData>(returnCode),
                             ERL_DRV_ATOM,  result,     ERL_DRV_LIST,  2,
                             ERL_DRV_TUPLE, 2,          ERL_DRV_TUPLE, 3};
    if (erl_drv_output_term(port, spec, sizeof(spec) / sizeof(spec[0])) < 0) {
        LOG_E(PORT_DRIVER_SUBJECT, "Failed outputting term");
    }
}

static std::unique_ptr<char[]> decode_string(char *buff, int *index) {
    int type;
    unsigned long entry_size;
    if (ei_get_type(buff, index, &type, reinterpret_cast<int *>(&entry_size)) != 0) {
        return nullptr;
    }
    if (type != ERL_BINARY_EXT && type != ERL_STRING_EXT && type != ERL_ATOM_EXT) {
        LOG_E(PORT_DRIVER_SUBJECT, "Wrong type %c, expected %c, %c, or %c", (char)type, ERL_BINARY_EXT, ERL_STRING_EXT,
              ERL_ATOM_EXT);
        return nullptr;
    }

    auto buf = std::unique_ptr<char[]>{nullptr};
    try {
        buf.reset(new char[entry_size + 1]);
    } catch (...) {
        LOG_E(PORT_DRIVER_SUBJECT, "Out of memory allocating %lu", entry_size + 1);
        return nullptr;
    }
    switch (type) {
    case ERL_BINARY_EXT: {
        // Zero out the memory as decode_binary won't insert the null char
        memset((void *)buf.get(), 0, sizeof(char) * (entry_size + 1));
        if (ei_decode_binary(buff, index, (void *)buf.get(), reinterpret_cast<long *>(&entry_size)) != 0) {
            LOG_E(PORT_DRIVER_SUBJECT, "Failed decoding binary");
            return nullptr;
        }
        break;
    }
    case ERL_STRING_EXT: {
        if (ei_decode_string(buff, index, (char *)buf.get()) != 0) {
            LOG_E(PORT_DRIVER_SUBJECT, "Failed decoding string");
            return nullptr;
        }
        break;
    }
    case ERL_ATOM_EXT: {
        if (ei_decode_atom(buff, index, buf.get()) != 0) {
            LOG_E(PORT_DRIVER_SUBJECT, "Failed decoding atom");
            return nullptr;
        }
        break;
    }
    default: {
        LOG_W(PORT_DRIVER_SUBJECT, "Missing branch decoding string");
        return nullptr;
    }
    }
    return buf;
}

static void handle_client_id_and_pem(DriverContext *context, char *buff, int index,
                                     bool (ClientDeviceAuthIntegration::*func)(const char *clientId, const char *pem)) {
    char return_code = RETURN_CODE_UNEXPECTED;
    ErlDrvTermData result_atom = ATOMS.unknown;

    defer { write_atom_to_port(context, result_atom, return_code); };

    auto client_id = decode_string(buff, &index);
    if (!client_id) {
        return;
    }

    auto pem = decode_string(buff, &index);
    if (!pem) {
        return;
    }

    LOG_D(PORT_DRIVER_SUBJECT, "Handling request with client id %s", client_id.get())
    bool result = (context->cda_integration->*func)(client_id.get(), pem.get());
    result_atom = result ? ATOMS.pass : ATOMS.fail;
    return_code = RETURN_CODE_SUCCESS;
}

static void handle_get_auth_token(DriverContext *context, char *buff, int index) {
    char return_code = RETURN_CODE_UNEXPECTED;
    std::unique_ptr<std::string> result = std::make_unique<std::string>("");
    defer { write_string_to_port(context, *result, return_code); };

    auto client_id = decode_string(buff, &index);
    if (!client_id) {
        return;
    }

    auto pem = decode_string(buff, &index);
    if (!pem) {
        return;
    }

    LOG_D(PORT_DRIVER_SUBJECT, "Handling get_auth_token request with client id %s", client_id.get());
    result = context->cda_integration->get_client_device_auth_token(client_id.get(), pem.get());
    if (result) {
        return_code = RETURN_CODE_SUCCESS;
    } else {
        result = std::make_unique<std::string>("");
    }
}

static void handle_check_acl(DriverContext *context, char *buff, int index) {
    char return_code = RETURN_CODE_UNEXPECTED;
    ErlDrvTermData result_atom = ATOMS.unknown;

    defer { write_atom_to_port(context, result_atom, return_code); };

    auto client_id = decode_string(buff, &index);
    if (!client_id) {
        return;
    }

    auto auth_token = decode_string(buff, &index);
    if (!auth_token) {
        return;
    }

    auto resource = decode_string(buff, &index);
    if (!resource) {
        return;
    }

    auto operation = decode_string(buff, &index);
    if (!operation) {
        return;
    }

    LOG_D(PORT_DRIVER_SUBJECT,
          "Handling acl request with client id %s, client token %s, for topic %s, and "
          "action %s",
          client_id.get(), auth_token.get(), resource.get(), operation.get())
    bool result =
        context->cda_integration->on_check_acl(client_id.get(), auth_token.get(), resource.get(), operation.get());

    result_atom = result ? ATOMS.authorized : ATOMS.unauthorized;
    return_code = RETURN_CODE_SUCCESS;
}

static void handle_verify_client_certificate(DriverContext *context, char *buff, int index) {
    char return_code = RETURN_CODE_UNEXPECTED;
    ErlDrvTermData result_atom = ATOMS.unknown;

    defer { write_atom_to_port(context, result_atom, return_code); };
    auto cert_pem = decode_string(buff, &index);
    if (!cert_pem) {
        return;
    }

    LOG_D(PORT_DRIVER_SUBJECT, "Handling verify_client_certificate request")
    bool result = context->cda_integration->verify_client_certificate(cert_pem.get());
    result_atom = result ? ATOMS.valid : ATOMS.invalid;
    return_code = RETURN_CODE_SUCCESS;
}

void handle_certificate_update_subscription(DriverContext *context) {
    char return_code = RETURN_CODE_UNEXPECTED;
    ErlDrvTermData result_atom = ATOMS.unknown;

    defer { write_atom_to_port(context, result_atom, return_code); };
    const char *baseDir = std::getenv(EMQX_DATA_ENV_VAR);
    if (baseDir == nullptr) {
        LOG_E(PORT_DRIVER_SUBJECT, "Environment variable %s was not set", EMQX_DATA_ENV_VAR);
        return;
    }
    auto baseDirPath = std::make_unique<std::filesystem::path>(baseDir);

    auto callback = std::make_unique<std::function<void(Aws::Greengrass::CertificateUpdateEvent *)>>(
        [context]([[maybe_unused]] Aws::Greengrass::CertificateUpdateEvent *event) {
            send_event_to_port(context, ATOMS.certificate_update);
        });
    CertSubscribeUpdateStatus result =
        context->cda_integration->subscribeToCertUpdates(std::move(baseDirPath), std::move(callback));
    result_atom = result == CertSubscribeUpdateStatus::SUBSCRIBE_SUCCESS ? ATOMS.valid : ATOMS.invalid;
    return_code = RETURN_CODE_SUCCESS;
}

struct packer {
    std::unique_ptr<char[]> client_id;
    std::unique_ptr<char[]> pem;
    DriverContext *context;
    ErlDrvTermData result = ATOMS.fail;
    char returnCode = RETURN_CODE_UNEXPECTED;
    EI_LONGLONG requestId;
};

void client_connect(void *buf) {
    auto *pack = reinterpret_cast<packer *>(buf);

    LOG_D(PORT_DRIVER_SUBJECT, "Handling request with client id %s", pack->client_id.get());
    bool result = pack->context->cda_integration->on_client_connect(pack->client_id.get(), pack->pem.get());
    if (result) {
        pack->result = ATOMS.pass;
    }
    pack->returnCode = RETURN_CODE_SUCCESS;
}

void client_connect_complete(void *buf) {
    auto *pack = reinterpret_cast<packer *>(buf);

    defer { delete pack; };

    write_async_bool_to_port(pack->context, pack->requestId, pack->result, pack->returnCode);
}

void handle_on_client_connect(DriverContext *context, char *buff, int index) {
    char return_code = RETURN_CODE_UNEXPECTED;
    ErlDrvTermData result = ATOMS.unknown;

    defer { write_atom_to_port(context, result, return_code); };
    auto client_id = decode_string(buff, &index);
    if (!client_id) {
        return;
    }
    auto pem = decode_string(buff, &index);
    if (!pem) {
        return;
    }
    auto *packed = new packer{
        .client_id = std::move(client_id),
        .pem = std::move(pem),
        .context = context,
    };
    // Decode the request ID which is used to identify the caller back in Erlang
    if (ei_decode_longlong(buff, &index, &packed->requestId) != 0) {
        return;
    }
    driver_async(context->port, nullptr, &client_connect, packed, &client_connect_complete);
    result = ATOMS.invalid;
    return_code = RETURN_CODE_ASYNC;
}

static void handle_unknown_op(DriverContext *context) {
    write_atom_to_port(context, ATOMS.unknown, RETURN_CODE_UNKNOWN_OP);
}

static bool decode_operation_header(const char *buff, int *index, unsigned long *op) {
    // Input must look like: [{OperationULong, ...Operation specific inputs}]

    int version = 0;
    // Read out the version header. The version doesn't matter to us, but we need
    // to read it to advance the index.
    if (ei_decode_version(buff, index, &version) != 0) {
        return false;
    }
    int arity = 0;

    // Decode the first thing, which must be a list
    if (ei_decode_list_header(buff, index, &arity) < 0) {
        return false;
    }
    // Decode the first member of the list which must be a tuple
    if (ei_decode_tuple_header(buff, index, &arity) < 0) {
        return false;
    }
    // Decode the first member of the tuple which must be the operation
    if (ei_decode_ulong(buff, index, op) < 0) {
        return false;
    }
    return true;
}

void drv_output(ErlDrvData handle, ErlIOVec *ev) {
    // Input must look like: [{OperationUlong, ...Operation specific inputs}]
    auto *context = reinterpret_cast<DriverContext *>(handle);
    ErlDrvBinary *bin = ev->binv[1];
    char *buff = &bin->orig_bytes[0];
    int index = 0;
    unsigned long op;
    if (!decode_operation_header(buff, &index, &op)) {
        LOG_E(PORT_DRIVER_SUBJECT, "Failed to parse operation input");
        handle_unknown_op(context);
        return;
    }

    switch (op) {
    case ON_CLIENT_CONNECT: {
        LOG_I(PORT_DRIVER_SUBJECT, "ON_CLIENT_CONNECT")
        handle_on_client_connect(context, buff, index);
        break;
    }
    case ON_CLIENT_CONNECTED:
        LOG_I(PORT_DRIVER_SUBJECT, "ON_CLIENT_CONNECTED")
        handle_client_id_and_pem(context, buff, index, &ClientDeviceAuthIntegration::on_client_connected);
        break;
    case ON_CLIENT_DISCONNECT:
        LOG_I(PORT_DRIVER_SUBJECT, "ON_CLIENT_DISCONNECT")
        handle_client_id_and_pem(context, buff, index, &ClientDeviceAuthIntegration::on_client_disconnected);
        break;
    case GET_CLIENT_DEVICE_AUTH_TOKEN:
        LOG_I(PORT_DRIVER_SUBJECT, "GET_CLIENT_DEVICE_AUTH_TOKEN")
        handle_get_auth_token(context, buff, index);
        break;
    case ON_CLIENT_CHECK_ACL:
        LOG_I(PORT_DRIVER_SUBJECT, "ON_CLIENT_CHECK_ACL")
        handle_check_acl(context, buff, index);
        break;
    case VERIFY_CLIENT_CERTIFICATE:
        LOG_I(PORT_DRIVER_SUBJECT, "VERIFY_CLIENT_CERTIFICATE")
        handle_verify_client_certificate(context, buff, index);
        break;
    case SUBSCRIBE_TO_CERTIFICATE_UPDATES:
        LOG_I(PORT_DRIVER_SUBJECT, "SUBSCRIBE_TO_CERTIFICATE_UPDATES")
        handle_certificate_update_subscription(context);
        break;
    default:
        LOG_E(PORT_DRIVER_SUBJECT, "Unknown operation: %lu", op);
        handle_unknown_op(context);
    }
}
