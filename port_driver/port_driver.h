/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
#pragma once

#include <erl_driver.h>

#if defined(_WIN32)
#include <basetsd.h>
#include <cstring> // need memcpy for the DRIVER_INIT macro
typedef SSIZE_T ssize_t;
#define EXPORTED __declspec(dllexport)
#else

#include <sys/types.h>
#define EXPORTED static
#endif

#include <ei.h>

// OPERATIONS
#define GET_CLIENT_DEVICE_AUTH_TOKEN 3
#define ON_CLIENT_CHECK_ACL 4
#define VERIFY_CLIENT_CERTIFICATE 5
#define SUBSCRIBE_TO_CERTIFICATE_UPDATES 6
#define SUBSCRIBE_TO_CONFIGURATION_UPDATES 7
#define GET_CONFIGURATION 8
#define UNSUBSCRIBE_FROM_CERTIFICATE_UPDATES 9

// RETURN CODES
#define RETURN_CODE_SUCCESS 0
#define RETURN_CODE_FAILED_OP 1
#define RETURN_CODE_UNKNOWN_OP 2
#define RETURN_CODE_UNEXPECTED 3
#define RETURN_CODE_ASYNC 100

#ifdef __cplusplus
extern "C" {
#endif
EXPORTED ErlDrvData drv_start(ErlDrvPort port, char *buff);
EXPORTED void drv_stop(ErlDrvData handle);
EXPORTED void drv_output(ErlDrvData handle, ErlIOVec *ev);

ErlDrvEntry driver_entry = {
    nullptr,                        /* F_PTR init, called when driver is loaded */
    drv_start,                      /* L_PTR start, called when port is opened */
    drv_stop,                       /* F_PTR stop, called when port is closed */
    nullptr,                        /* F_PTR output, called when erlang has sent */
    nullptr,                        /* F_PTR ready_input, called when input descriptor ready */
    nullptr,                        /* F_PTR ready_output, called when output descriptor ready */
    (char *)"port_driver",          /* char *driver_name, the argument to open_port */
    nullptr,                        /* F_PTR finish, called when unloaded */
    nullptr,                        /* void *handle, Reserved by VM */
    nullptr,                        /* F_PTR control, port_command callback */
    nullptr,                        /* F_PTR timeout, reserved */
    drv_output,                     /* F_PTR outputv, reserved */
    nullptr,                        /* F_PTR ready_async, only for async drivers */
    nullptr,                        /* F_PTR flush, called when port is about to be closed,
                                but there is data in driver
                                queue */
    nullptr,                        /* F_PTR call, much like control, sync call to driver */
    nullptr,                        /* unused */
    (int)ERL_DRV_EXTENDED_MARKER,   /* int extended marker, Should always be set
                                       to indicate driver versioning */
    ERL_DRV_EXTENDED_MAJOR_VERSION, /* int major_version, should always be set
                                       to this value */
    ERL_DRV_EXTENDED_MINOR_VERSION, /* int minor_version, should always be set
                                       to this value */
    0,                              /* int driver_flags, see documentation */
    nullptr,                        /* void *handle2, reserved for VM use */
    nullptr,                        /* F_PTR process_exit, called when a monitored process dies */
    nullptr,                        /* F_PTR stop_select, called to close an event object */
    nullptr,                        /* F_PTR emergency_close, called when the port is closed abruptly
                                                   specifically when erl_crash_dump is called */
};

DRIVER_INIT(port_driver) /* must match name in driver_entry */
{
    return &driver_entry;
}
#ifdef __cplusplus
}
#endif
