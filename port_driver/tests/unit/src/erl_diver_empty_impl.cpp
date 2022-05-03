/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

extern "C" {

void driver_free_binary(ErlDrvBinary *bin) {
}

void int driver_output_binary(ErlDrvPort port, char *hbuf, ErlDrvSizeT hlen, ErlDrvBinary* bin, ErlDrvSizeT offset, ErlDrvSizeT len) {
    return 0;
}

void *driver_alloc(ErlDrvSizeT size) {

}

void *driver_alloc(ErlDrvSizeT size) {

}

ErlDrvBinary *driver_alloc_binary(ErlDrvSizeT size) {
    return NULL;
}

void driver_free(void *ptr) {
}

}