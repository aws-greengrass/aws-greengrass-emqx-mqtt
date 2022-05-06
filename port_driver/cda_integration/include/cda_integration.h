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

bool on_client_connect(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem);

bool on_client_connected(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem);

bool on_client_disconnected(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem);

bool on_client_authenticate(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem);

bool on_check_acl(CDA_INTEGRATION_HANDLE *handle, const char *clientId, const char *pem, const char *topic,
                  const char *action);

bool cda_integration_close(CDA_INTEGRATION_HANDLE *handle);
