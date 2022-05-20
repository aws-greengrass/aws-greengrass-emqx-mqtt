/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#pragma once

#include <aws/greengrass/GreengrassCoreIpcClient.h>

namespace GG = Aws::Greengrass;

static const std::string filePath = "testCerts";
static const std::string privateKeyFilePath = filePath + "/key.pem";
static const std::string certFilePath = filePath + "/cert.pem";
static const std::string caCertFilePath = filePath + "/cacert.pem";
static const std::string testPrivateKey = "testPrivateKey";
static const std::string testCert = "testCert";
static const std::string testCACert = "testCACert";

static const void delete_file(std::string fileName) {
    if (!std::filesystem::remove(fileName)) {
        std::cout << "Failed to delete " << fileName << std::endl;
    }
}

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
