/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <cstdio>

#include "defer.h"
#include "port_driver.h"
#include <aws/crt/Api.h>
#include <cda_integration.h>
#include <cstring>
#include <memory>

enum log_subject {
    PORT_DRIVER_SUBJECT = AWS_LOG_SUBJECT_BEGIN_RANGE(100),
};

#define LOG(...) AWS_LOGF_INFO(PORT_DRIVER_SUBJECT, __VA_ARGS__)

typedef struct {
    ErlDrvPort port;
    CDA_INTEGRATION_HANDLE *cda_integration_handle;
} DriverContext;

static struct aws_logger our_logger{};

struct atoms {
    ErlDrvTermData data;
    ErlDrvTermData true_;
    ErlDrvTermData false_;
};
static struct atoms ATOMS{};

EXPORTED ErlDrvData drv_start(ErlDrvPort port, char *buff) {
    (void) buff;
    struct aws_logger_standard_options logger_options = {
            .level = AWS_LL_TRACE,
            .filename = "crt.log",
    };
    aws_logger_init_standard(&our_logger, aws_default_allocator(), &logger_options);
    aws_logger_set(&our_logger);

    LOG("Starting AWS Greengrass auth driver");
    ei_init();

    // Setup static atoms
    ATOMS.data = driver_mk_atom((char *) "data");
    ATOMS.true_ = driver_mk_atom((char *) "true");
    ATOMS.false_ = driver_mk_atom((char *) "false");

    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    auto *context = (DriverContext *) driver_alloc(sizeof(DriverContext));
    context->port = port;
    context->cda_integration_handle = cda_integration_init();
    if (context->cda_integration_handle == nullptr) {
        LOG("Failed to initialize CDA handle");

        // return value -1 means failure
        return (ErlDrvData) -1;
    }
    return (ErlDrvData) context;
}

EXPORTED void drv_stop(ErlDrvData handle) {
    LOG("Stopping AWS Greengrass auth driver");
    auto *context = (DriverContext *) handle;
    cda_integration_close(context->cda_integration_handle);
    driver_free((char *) handle);
    aws_logger_clean_up(&our_logger);
}

static void write_bool_to_port(DriverContext *context, bool result, const char return_code) {
    auto port = driver_mk_port(context->port);

    // https://www.erlang.org/doc/man/erl_driver.html#erl_drv_output_term
    // The follow code encodes this Erlang term: {Port, {data, [return code integer, true or false atom]}}

    ErlDrvTermData spec[] = {
            ERL_DRV_PORT, port,
            ERL_DRV_ATOM, ATOMS.data,
            ERL_DRV_INT, (ErlDrvTermData) return_code,
            ERL_DRV_ATOM, result ? ATOMS.true_ : ATOMS.false_,
            ERL_DRV_LIST, 2,
            ERL_DRV_TUPLE, 2,
            ERL_DRV_TUPLE, 2
    };
    if (erl_drv_output_term(port, spec, sizeof(spec) / sizeof(spec[0])) < 0) {
        LOG("Failed outputting term");
    }
}

static void
write_async_bool_to_port(DriverContext *context, EI_LONGLONG requestId, bool result, const char returnCode) {
    auto port = driver_mk_port(context->port);

    // https://www.erlang.org/doc/man/erl_driver.html#erl_drv_output_term
    // The follow code encodes this Erlang term: {Port, request id integer, {data, [return code integer, true or false atom]}}

    ErlDrvTermData spec[] = {
            ERL_DRV_PORT, port,
            ERL_DRV_INT64, (ErlDrvTermData) &requestId,
            ERL_DRV_ATOM, ATOMS.data,
            ERL_DRV_INT, (ErlDrvTermData) returnCode,
            ERL_DRV_ATOM, result ? ATOMS.true_ : ATOMS.false_,
            ERL_DRV_LIST, 2,
            ERL_DRV_TUPLE, 2,
            ERL_DRV_TUPLE, 3
    };
    if (erl_drv_output_term(port, spec, sizeof(spec) / sizeof(spec[0])) < 0) {
        LOG("Failed outputting term");
    }
}

static std::unique_ptr<char[]> decode_string(char *buff, int *index) {
    int type;
    int entry_size;
    if (ei_get_type(buff, index, &type, &entry_size)) {
        return nullptr;
    }
    if (type != ERL_BINARY_EXT && type != ERL_STRING_EXT && type != ERL_ATOM_EXT) {
        LOG("Wrong type %c, expected %c, %c, or %c", (char) type, ERL_BINARY_EXT, ERL_STRING_EXT, ERL_ATOM_EXT);
        return nullptr;
    }

    auto b = std::unique_ptr<char[]>{nullptr};
    try {
        b.reset(new char[entry_size + 1]);
    } catch (...) {
        LOG("Out of memory allocating %d", entry_size + 1);
        return nullptr;
    }
    switch (type) {
        case ERL_BINARY_EXT: {
            // Zero out the memory as decode_binary won't insert the null char
            memset((void *) b.get(), 0, sizeof(char) * (entry_size + 1));
            if (ei_decode_binary(buff, index, (void *) b.get(), (long *) &entry_size)) {
                LOG("Failed decoding binary");
                return nullptr;
            }
            break;
        }
        case ERL_STRING_EXT: {
            if (ei_decode_string(buff, index, (char *) b.get())) {
                LOG("Failed decoding string");
                return nullptr;
            }
            break;
        }
        case ERL_ATOM_EXT: {
            if (ei_decode_atom(buff, index, b.get())) {
                LOG("Failed decoding atom");
                return nullptr;
            }
            break;
        }
        default: {
            LOG("Missing branch decoding string");
            return nullptr;
        }
    }
    return b;
}

static void handle_client_id_and_pem(DriverContext *context, char *buff,
                                     int index,
                                     bool (*func)(CDA_INTEGRATION_HANDLE *handle, const char *clientId,
                                                  const char *pem)) {
    char return_code = RETURN_CODE_UNEXPECTED;
    bool result = false;

    defer {
        write_bool_to_port(context, result, return_code);
    };

    auto client_id = decode_string(buff, &index);
    if (!client_id) {
        return;
    }

    auto pem = decode_string(buff, &index);
    if (!pem) {
        return;
    }
    LOG("Handling request with client id %s, pem %s", client_id.get(), pem.get())
    result = (*func)(context->cda_integration_handle, client_id.get(), pem.get());
    return_code = RETURN_CODE_SUCCESS;
}

static void handle_check_acl(DriverContext *context, char *buff, int index) {
    char return_code = RETURN_CODE_UNEXPECTED;
    bool result = false;

    defer {
        write_bool_to_port(context, result, return_code);
    };

    auto client_id = decode_string(buff, &index);
    if (!client_id) { return; }

    auto pem = decode_string(buff, &index);
    if (!pem) { return; }

    auto topic = decode_string(buff, &index);
    if (!topic) { return; }

    auto pub_sub = decode_string(buff, &index);
    if (!pub_sub) { return; }

    LOG("Handling acl request with client id %s, pem %s, for topic %s, and action %s",
        client_id.get(), pem.get(), topic.get(), pub_sub.get())
    result = on_check_acl(context->cda_integration_handle, client_id.get(),
                          pem.get(), topic.get(),
                          pub_sub.get());
    return_code = RETURN_CODE_SUCCESS;
}

struct packer {
    std::unique_ptr<char[]> client_id;
    std::unique_ptr<char[]> pem;
    DriverContext *context;
    bool result = false;
    char returnCode = RETURN_CODE_UNEXPECTED;
    EI_LONGLONG requestId;
};

void client_connect(void *buf) {
    auto pack = (packer *) buf;

    LOG("Handling request with client id %s, pem %s", pack->client_id.get(), pack->pem.get());
    bool result = on_client_connect(pack->context->cda_integration_handle, pack->client_id.get(), pack->pem.get());
    pack->result = result;
    pack->returnCode = RETURN_CODE_SUCCESS;
}

void client_connect_complete(void *buf) {
    auto pack = (packer *) buf;

    defer {
        delete pack;
    };

    write_async_bool_to_port(pack->context, pack->requestId, pack->result, pack->returnCode);
}

void handle_on_client_connect(DriverContext *context, char *buff, int index) {
    char return_code = RETURN_CODE_UNEXPECTED;
    bool result = false;

    defer {
        write_bool_to_port(context, result, return_code);
    };
    auto client_id = decode_string(buff, &index);
    if (!client_id) {
        return;
    }
    auto pem = decode_string(buff, &index);
    if (!pem) {
        return;
    }
    auto packed = new packer{
            .client_id = std::move(client_id),
            .pem = std::move(pem),
            .context = context,
    };
    // Decode the request ID which is used to identify the caller back in Erlang
    if (ei_decode_longlong(buff, &index, &packed->requestId) != 0) {
        return;
    }
    driver_async(context->port, nullptr, &client_connect, packed, &client_connect_complete);
    result = true;
    return_code = RETURN_CODE_ASYNC;
}

static void handle_unknown_op(DriverContext *context) {
    bool result = false;
    write_bool_to_port(context, result, RETURN_CODE_UNKNOWN_OP);
}

static bool decode_operation_header(const char *buff, int *index, unsigned long *op) {
    // Input must look like: [{OperationULong, ...Operation specific inputs}]

    int version = 0;
    // Read out the version header. The version doesn't matter to us, but we need to read it to advance the index.
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
    auto *context = (DriverContext *) handle;
    ErlDrvBinary *bin = ev->binv[1];
    char *buff = &bin->orig_bytes[0];
    int index = 0;
    unsigned long op;
    if (!decode_operation_header(buff, &index, &op)) {
        LOG("Failed to parse operation input");
        handle_unknown_op(context);
        return;
    }

    switch (op) {
        case ON_CLIENT_CONNECT: {
            LOG("ON_CLIENT_CONNECT")
            handle_on_client_connect(context, buff, index);
            break;
        }
        case ON_CLIENT_CONNECTED: LOG("ON_CLIENT_CONNECTED")
            handle_client_id_and_pem(context, buff, index, &on_client_connected);
            break;
        case ON_CLIENT_DISCONNECT: LOG("ON_CLIENT_DISCONNECT")
            handle_client_id_and_pem(context, buff, index, &on_client_disconnected);
            break;
        case ON_CLIENT_AUTHENTICATE: LOG("ON_CLIENT_AUTHENTICATE")
            handle_client_id_and_pem(context, buff, index, &on_client_authenticate);
            break;
        case ON_CLIENT_CHECK_ACL: LOG("ON_CLIENT_CHECK_ACL")
            handle_check_acl(context, buff, index);
            break;
        default: LOG("Unknown operation: %lu", op);
            handle_unknown_op(context);
    }
}
