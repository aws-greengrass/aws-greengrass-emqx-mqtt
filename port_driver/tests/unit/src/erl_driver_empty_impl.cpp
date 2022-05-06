/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <erl_driver.h>

/*
 Library erl_driver is not available to link against.
 It is available at runtime when running in erlang runtime.
*/

// Need empty impl for erl_driver to make ld (linux) happy while linking unit tests
#ifndef _WIN32

extern "C" {

void *driver_alloc(ErlDrvSizeT size) {
    (void)size;
    return NULL;
}

void driver_free(void *ptr) { (void)ptr; }

ErlDrvTermData driver_mk_atom(char *string) {
    (void)string;
    return 0;
}

ErlDrvTermData driver_mk_port(ErlDrvPort port) {
    (void)port;
    return 0;
}

int erl_drv_output_term(ErlDrvTermData port, ErlDrvTermData *term, int n) {
    (void)port;
    (void)term;
    (void)n;
    return 0;
}

void set_port_control_flags(ErlDrvPort port, int flags) {
    (void)port;
    (void)flags;
}

long driver_async([[maybe_unused]] ErlDrvPort port, [[maybe_unused]] unsigned int *key, void (*async_invoke)(void *),
                  void *async_data, void (*async_free)(void *)) {
    if (async_invoke != nullptr) {
        async_invoke(async_data);
    }
    if (async_free != nullptr) {
        async_free(async_data);
    }
    return 0;
}
}
#endif
