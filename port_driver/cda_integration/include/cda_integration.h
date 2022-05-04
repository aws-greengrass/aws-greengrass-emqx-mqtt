/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#pragma once

/*
 * @brief Handle to client device auth integration
 */
typedef void *CDA_INTEGRATION_HANDLE;

CDA_INTEGRATION_HANDLE *cda_integration_init();

enum OperationResult {
    FAIL    = 0, 
    PASS    = 1, 
    UNKNOWN = 2
};

OperationResult on_client_connect(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem);

OperationResult on_client_connected(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem);

OperationResult on_client_disconnected(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem);

OperationResult on_client_authenticate(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem);

OperationResult on_check_acl(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem,
                  const char *topic, const char *action);

OperationResult verify_client_certificate(CDA_INTEGRATION_HANDLE *handle, const char *certPem);

OperationResult cda_integration_close(CDA_INTEGRATION_HANDLE *handle);
