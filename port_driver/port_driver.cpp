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

EXPORTED ErlDrvData drv_start(ErlDrvPort port, char *buff) {
    (void) buff;
    struct aws_logger_standard_options logger_options = {
            .level = AWS_LL_TRACE,
            .filename = "crt.log",
    };
    aws_logger_init_standard(&our_logger, aws_default_allocator(), &logger_options);
    aws_logger_set(&our_logger);

    LOG("Starting the driver");
    ei_init();
    auto *context = (DriverContext *) driver_alloc(sizeof(DriverContext));
    context->port = port;
    context->cda_integration_handle = cda_integration_init();
    return (ErlDrvData) context;
}

EXPORTED void drv_stop(ErlDrvData handle) {
    LOG("Stopping the driver");
    auto *context = (DriverContext *) handle;
    cda_integration_close(context->cda_integration_handle);
    driver_free((char *) handle);
    aws_logger_clean_up(&our_logger);
}

static unsigned int get_operation(ErlIOVec *ev) {
    ErlDrvBinary *bin = ev->binv[1];
    return bin->orig_bytes[0];
}

static void write_bool_to_port(DriverContext *context, bool result, const char return_code) {
    ErlDrvBinary *out = driver_alloc_binary(sizeof(result));
    if (!out) {
        LOG("Out of memory");
        return;
    }

    memcpy(&out->orig_bytes[0], &result, sizeof(result));
    char return_code_temp = return_code;
    if (driver_output_binary(context->port, &return_code_temp, 1, out, 0, sizeof(result))) {
        LOG("Out of memory");
        driver_free_binary(out);
    }

    driver_free_binary(out);
}

static int decode_version(char *buff, int *index) {
    // read and discard
    int version = 0;
    if (ei_decode_version(buff, index, &version)) {
        return -1;
    }
    return version;
}

static int get_next_entry_size(char *buff, int *index) {
    // decode and discard
    if (decode_version(buff, index) == -1) {
        return -1;
    }

    int type;
    int entry_size;
    if (ei_get_type(buff, index, &type, &entry_size)) {
        return -1;
    }
    return entry_size;
}

static char *get_buffer_for_next_entry(char *buff, int *index) {
    int entry_size = get_next_entry_size(buff, index);
    if (entry_size == -1) {
        LOG("Failed to get the entry size of next entry");
        return nullptr;
    }

    char *b = (char *) malloc(sizeof(char) * (entry_size + 1));
    if (!b) {
        LOG("Out of memory");
    }
    return b;
}

static void handle_client_id_and_pem(DriverContext *context, ErlIOVec *ev,
                                     bool (*func)(CDA_INTEGRATION_HANDLE *handle, const char *clientId,
                                                  const char *pem)) {
    char return_code = RETURN_CODE_UNEXPECTED;
    bool result = false;

    ErlDrvBinary *bin = ev->binv[1];
    char *buff = &bin->orig_bytes[1];

    defer {
        write_bool_to_port(context, result, return_code);
    };

    int index = 0;
    auto client_id = std::unique_ptr<char>{get_buffer_for_next_entry(buff, &index)};
    if (!client_id || ei_decode_string(buff, &index, client_id.get()) != 0) {
        return;
    }

    auto pem = std::unique_ptr<char>{get_buffer_for_next_entry(buff, &index)};
    if (!pem || ei_decode_string(buff, &index, pem.get()) != 0) {
        return;
    }
    LOG("Handling request with client id %s, pem %s", client_id.get(), pem.get())
    result = (*func)(context->cda_integration_handle, client_id.get(), pem.get());
    return_code = RETURN_CODE_SUCCESS;
}

static void handle_check_acl(DriverContext *context, ErlIOVec *ev) {
    char return_code = RETURN_CODE_UNEXPECTED;
    bool result = false;

    ErlDrvBinary *bin = ev->binv[1];
    char *buff = &bin->orig_bytes[1];

    defer {
        write_bool_to_port(context, result, return_code);
    };

    int index = 0;
    auto client_id = std::unique_ptr<char>{get_buffer_for_next_entry(buff, &index)};
    if (!client_id || ei_decode_string(buff, &index, client_id.get()) != 0) { return; }

    auto pem = std::unique_ptr<char>{get_buffer_for_next_entry(buff, &index)};
    if (!pem || ei_decode_string(buff, &index, pem.get()) != 0) { return; }

    auto topic = std::unique_ptr<char>{get_buffer_for_next_entry(buff, &index)};
    if (!topic || ei_decode_string(buff, &index, topic.get()) != 0) { return; }

    auto pub_sub = std::unique_ptr<char>{get_buffer_for_next_entry(buff, &index)};
    if (!pub_sub || ei_decode_string(buff, &index, pub_sub.get()) != 0) { return; }

    LOG("Handling acl request with client id %s, pem %s, for topic %s, and action %s",
        client_id.get(), pem.get(), topic.get(), pub_sub.get())
    result = on_check_acl(context->cda_integration_handle, client_id.get(),
                          pem.get(), topic.get(),
                          pub_sub.get());
    return_code = RETURN_CODE_SUCCESS;
}

static void handle_unknown_op(DriverContext *context) {
    bool result = false;
    write_bool_to_port(context, result, RETURN_CODE_UNKNOWN_OP);
}

EXPORTED void drv_output(ErlDrvData handle, ErlIOVec *ev) {
    auto *context = (DriverContext *) handle;
    const unsigned int op = get_operation(ev);
    switch (op) {
        case ON_CLIENT_CONNECT: LOG("ON_CLIENT_CONNECT")
            handle_client_id_and_pem(context, ev, &on_client_connect);
            break;
        case ON_CLIENT_CONNECTED: LOG("ON_CLIENT_CONNECTED")
            handle_client_id_and_pem(context, ev, &on_client_connected);
            break;
        case ON_CLIENT_DISCONNECT: LOG("ON_CLIENT_DISCONNECT")
            handle_client_id_and_pem(context, ev, &on_client_disconnected);
            break;
        case ON_CLIENT_AUTHENTICATE: LOG("ON_CLIENT_AUTHENTICATE")
            handle_client_id_and_pem(context, ev, &on_client_authenticate);
            break;
        case ON_CLIENT_CHECK_ACL: LOG("ON_CLIENT_CHECK_ACL")
            handle_check_acl(context, ev);
            break;
        default: LOG("Unknown operation: %u", op);
            handle_unknown_op(context);
    }
}
