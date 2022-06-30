/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#pragma once

#include <logger.h>
#include <string>
#include <aws/greengrass/GreengrassCoreIpcClient.h>

namespace GG = Aws::Greengrass;

static const std::string filePath = "testCerts";
static const std::string privateKeyFilePath = filePath + "/key.pem";
static const std::string certFilePath = filePath + "/cert.pem";
static const std::string caCertFilePath = filePath + "/cacert.pem";
static const std::string testPrivateKey = "testPrivateKey";
static const std::string testCert = "testCert";
static const std::string testCACert = "testCACert";

static struct aws_logger our_logger {};
static struct aws_logger_standard_options logger_options = {
        .level = AWS_LL_WARN,
        .file = stderr,
};

static const void delete_file(std::string fileName) {
    if (!std::filesystem::remove(fileName)) {
        std::cout << "Failed to delete " << fileName << std::endl;
    }
}

[[maybe_unused]]
static const void delete_certs() {
    if (std::filesystem::exists(filePath)) {
        if (std::filesystem::exists(privateKeyFilePath)) {
            delete_file(privateKeyFilePath);
        }
        if (std::filesystem::exists(certFilePath)) {
            delete_file(certFilePath);
        }
        if (std::filesystem::exists(caCertFilePath)) {
            delete_file(caCertFilePath);
        }
        delete_file(filePath);
    }
}

static inline void rtrim(std::string &s) {
    s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char ch) {
        return !std::isspace(ch);
    }).base(), s.end());
}
