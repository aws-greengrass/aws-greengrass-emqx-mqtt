/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <cstdlib>

#include "port_driver.h"
#include <cda_integration.h>
#include <cstring>

// TODO: Improve logging. Add timestamp to the logs
#if defined(_MSC_VER)
#define LOG(fmt,...) printf("%s:%d %s() " fmt"\n",__FILE__,__LINE__,__func__, __VA_ARGS__)
#else
#define LOG(fmt, ...) printf("%s:%d %s() " fmt"\n",__FILE__,__LINE__,__func__, ##__VA_ARGS__)
#endif

typedef struct {
    ErlDrvPort port;
    CDA_INTEGRATION_HANDLE* cda_integration_handle;
} DriverContext;

EXPORTED ErlDrvData drv_start(ErlDrvPort port, char* buff)
{
    (void)buff;
    LOG("Starting the driver");
    ei_init();
    auto* context = (DriverContext*)driver_alloc(sizeof(DriverContext));
    context->port = port;
    context->cda_integration_handle = cda_integration_init();
    return (ErlDrvData)context;
}

EXPORTED void drv_stop(ErlDrvData handle)
{
    LOG("Stopping the driver");
    auto* context = (DriverContext*)handle;
    cda_integration_close(context->cda_integration_handle);
    driver_free((char*)handle);
}

static unsigned int get_operation(ErlIOVec *ev) {
    ErlDrvBinary* bin = ev->binv[1];
    return bin->orig_bytes[0];
}

static void write_bool_to_port(DriverContext* context, bool result, const char return_code) {
    ErlDrvBinary* out = driver_alloc_binary(sizeof(result));
    if(!out) {
        LOG("Out of memory");
        goto cleanup;
    }
    {
        memcpy(&out->orig_bytes[0], &result, sizeof(result));
        char return_code_temp = return_code;
        if (driver_output_binary(context->port, &return_code_temp, 1, out, 0, sizeof(result))) {
            LOG("Out of memory");
            goto cleanup;
        }
    }

cleanup:
    driver_free_binary(out);
}

static int decode_version(char* buff, int* index) {
    // read and discard
    int version = 0;
    if(ei_decode_version(buff, index, &version)) {
        return -1;
    }
    return version;
}

static int get_next_entry_size(char* buff, int* index) {
    // decode and discard
    if(decode_version(buff, index) == -1) {
        return -1;
    }

    int type;
    int entry_size;
    if(ei_get_type(buff, index, &type, &entry_size)) {
        return -1;
    }
    return entry_size;
}

static char* get_buffer_for_next_entry(char* buff, int* index) {
    int entry_size = get_next_entry_size(buff, index);
    if(entry_size == -1) {
        LOG("Failed to get the entry size of next entry");
        return nullptr;
    }

    char* b = (char *)malloc(sizeof(char) * (entry_size + 1));
    if(!b) {
        LOG("Out of memory");
    }
    return b;
}

static void handle_client_id_and_pem(DriverContext* context, ErlIOVec *ev,
        bool (*func)(CDA_INTEGRATION_HANDLE* handle, const char* clientId, const char* pem)) {
    char *client_id, *pem = nullptr;
    char return_code = RETURN_CODE_SUCCESS;
    bool result = false;

    ErlDrvBinary* bin = ev->binv[1];
    char* buff = &bin->orig_bytes[1];

    int index = 0;
    client_id = get_buffer_for_next_entry(buff, &index);
    if(!client_id) {
        goto cleanup;
    }
    if(ei_decode_string(buff, &index, client_id)) {
        return_code = RETURN_CODE_UNEXPECTED;
        goto respond;
    }

    pem = get_buffer_for_next_entry(buff, &index);
    if(!pem) {
        goto cleanup;
    }
    if(ei_decode_string(buff, &index, pem)) {
        return_code = RETURN_CODE_UNEXPECTED;
        goto respond;
    }

    result = (*func)(context->cda_integration_handle, client_id, pem);

respond:
    write_bool_to_port(context, result, return_code);

cleanup:
    {
        if (client_id != nullptr)
            free(client_id);
        if (pem != nullptr)
            free(pem);
    }
}

static void handle_check_acl(DriverContext* context, ErlIOVec *ev) {
    char *client_id, *pem, *topic, *pub_sub = nullptr;
    char return_code = RETURN_CODE_SUCCESS;
    bool result = false;

    ErlDrvBinary* bin = ev->binv[1];
    char* buff = &bin->orig_bytes[1];

    int index = 0;
    client_id = get_buffer_for_next_entry(buff, &index);
    if(!client_id) {
        goto cleanup;
    }
    if(ei_decode_string(buff, &index, client_id)) {
        return_code = RETURN_CODE_UNEXPECTED;
        goto respond;
    }

    pem = get_buffer_for_next_entry(buff, &index);
    if(!pem) {
        goto cleanup;
    }
    if(ei_decode_string(buff, &index, pem)) {
        return_code = RETURN_CODE_UNEXPECTED;
        goto respond;
    }

    topic = get_buffer_for_next_entry(buff, &index);
    if(!topic) {
        goto cleanup;
    }
    if(ei_decode_string(buff, &index, topic)) {
        return_code = RETURN_CODE_UNEXPECTED;
        goto respond;
    }

    pub_sub = get_buffer_for_next_entry(buff, &index);
    if(!pub_sub) {
        goto cleanup;
    }
    if(ei_decode_string(buff, &index, pub_sub)) {
        return_code = RETURN_CODE_UNEXPECTED;
        goto respond;
    }

    result = on_check_acl(context->cda_integration_handle, client_id, pem, topic, pub_sub);

respond:
    write_bool_to_port(context, result, return_code);

cleanup:
    {
        if (client_id != nullptr)
            free(client_id);
        if (pem != nullptr)
            free(pem);
        if (topic != nullptr)
            free(topic);
        if (pub_sub != nullptr)
            free(pub_sub);
    }
}

static void handle_unknown_op(DriverContext* context) {
    bool result = false;
    write_bool_to_port(context, result, RETURN_CODE_UNKNOWN_OP);
}

EXPORTED void drv_output(ErlDrvData handle, ErlIOVec *ev)
{
    auto* context = (DriverContext*)handle;
    const unsigned int op = get_operation(ev);
    switch(op) {
        case ON_CLIENT_CONNECT:
            handle_client_id_and_pem(context, ev, &on_client_connect);
            break;
        case ON_CLIENT_CONNECTED:
            handle_client_id_and_pem(context, ev, &on_client_connected);
            break;
        case ON_CLIENT_DISCONNECT:
            handle_client_id_and_pem(context, ev, &on_client_disconnected);
            break;
        case ON_CLIENT_AUTHENTICATE:
            handle_client_id_and_pem(context, ev, &on_client_authenticate);
            break;
        case ON_CLIENT_CHECK_ACL:
            handle_check_acl(context, ev);
            break;
        default:
            LOG("Unknown operation: %u", op);
            handle_unknown_op(context);
    }
}
