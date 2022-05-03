/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <erl_driver.h>

/*
 Library erl_driver is not available to link against.
 It is available at runtime when running in erlang runtime.
*/

extern "C" {

void driver_free_binary(ErlDrvBinary *bin) {
    (void)bin;
}

int driver_output_binary(ErlDrvPort port, char *hbuf, ErlDrvSizeT hlen, ErlDrvBinary* bin, ErlDrvSizeT offset, ErlDrvSizeT len) {
    (void)port;
    (void)hbuf;
    (void)hlen;
    (void)bin;
    (void)offset;
    (void)len;
    return 0;
}

void *driver_alloc(ErlDrvSizeT size) {
    (void)size;
    return NULL;
}

ErlDrvBinary *driver_alloc_binary(ErlDrvSizeT size) {
    (void)size;
    return NULL;
}

void driver_free(void *ptr) {
    (void)ptr;
}

}
