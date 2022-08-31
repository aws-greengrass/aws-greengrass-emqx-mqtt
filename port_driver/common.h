#pragma once

#include <logger.h>

static struct aws_logger our_logger {};

static aws_log_level crtStringToLogLevel(const std::string &level) {
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

static aws_log_level erlangStringToLogLevel(const std::string &level) {
    // Erlang log levels
    // debug, info, notice, warning, error, critical, alert, emergency
    if (level == "debug") {
        return AWS_LL_TRACE;
    }
    if (level == "info") {
        return AWS_LL_DEBUG;
    }
    if (level == "notice") {
        return AWS_LL_INFO;
    }
    if (level == "warning") {
        return AWS_LL_WARN;
    }
    if (level == "error") {
        return AWS_LL_ERROR;
    }
    if (level == "emergency") {
        return AWS_LL_FATAL;
    }
    // Default to warn
    return AWS_LL_WARN;
}
