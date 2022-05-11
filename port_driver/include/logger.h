/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#pragma once

#include <aws/crt/Api.h>

enum log_subject {
    PORT_DRIVER_SUBJECT = AWS_LOG_SUBJECT_BEGIN_RANGE(100),
    CDA_INTEG_SUBJECT = AWS_LOG_SUBJECT_BEGIN_RANGE(101),
    CERT_UPDATER_SUBJECT = AWS_LOG_SUBJECT_BEGIN_RANGE(102),
    IPC_WRAPPER_SUBJECT = AWS_LOG_SUBJECT_BEGIN_RANGE(103),
};

#define LOG_I(subject, ...) AWS_LOGF_DEBUG(subject, __VA_ARGS__)
#define LOG_D(subject, ...) AWS_LOGF_DEBUG(subject, __VA_ARGS__)
#define LOG_W(subject, ...) AWS_LOGF_WARN(subject, __VA_ARGS__)
#define LOG_E(subject, ...) AWS_LOGF_ERROR(subject, __VA_ARGS__)
