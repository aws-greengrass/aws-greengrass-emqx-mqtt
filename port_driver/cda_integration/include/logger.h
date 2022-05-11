/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#pragma once

#include <aws/crt/Api.h>

enum log_subject {
    CDA_INTEG_SUBJECT = AWS_LOG_SUBJECT_BEGIN_RANGE(101),
};

#define LOG(...) AWS_LOGF_INFO(CDA_INTEG_SUBJECT, __VA_ARGS__)
