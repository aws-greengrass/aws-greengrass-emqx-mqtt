/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include "common.h"

aws_log_level crtStringToLogLevel(const std::string &level) {
    // Crt defined log levels
    if (level == "none") {
        return AWS_LL_NONE;
    }
    if (level == "trace") {
        return AWS_LL_TRACE;
    }
    if (level == "debug") {
        return AWS_LL_DEBUG;
    }
    if (level == "info") {
        return AWS_LL_INFO;
    }
    if (level == "warn") {
        return AWS_LL_WARN;
    }
    if (level == "error") {
        return AWS_LL_ERROR;
    }
    if (level == "fatal") {
        return AWS_LL_FATAL;
    }
    // Default to warn
    return AWS_LL_WARN;
}
