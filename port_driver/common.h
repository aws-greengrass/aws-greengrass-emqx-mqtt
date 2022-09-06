/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#pragma once

#include <logger.h>

static struct aws_logger our_logger {};

aws_log_level crtStringToLogLevel(const std::string &level);
aws_log_level erlangStringToLogLevel(const std::string &level);
