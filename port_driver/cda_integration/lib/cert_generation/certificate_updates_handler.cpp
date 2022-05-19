/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <aws/greengrass/GreengrassCoreIpcClient.h>
#include <filesystem>
#include <fstream>
#include <iostream>

#include "logger.h"
#include "private/certificate_updater.h"

#define SUBSCRIBE_TIMEOUT_SECONDS 10

// TODO: naming
static const std::filesystem::path EMQX_KEY_PATH = std::filesystem::path{"key.pem"};
static const std::filesystem::path EMQX_PEM_PATH = std::filesystem::path{"cert.pem"};
static const std::filesystem::path EMQX_CA_PATH = std::filesystem::path{"cacert.pem"};

int CertificateUpdatesHandler::writeCertsToFiles(
    Aws::Crt::String &privateKeyValue, Aws::Crt::String &certValue,
    std::vector<Aws::Crt::String, Aws::Crt::StlAllocator<Aws::Crt::String>> &allCAsValue) {

    if (!basePath) {
        return -1;
    }

    const auto path = *basePath;
    try {
        if (std::filesystem::create_directories(path)) {
            LOG_D(CERT_UPDATER_SUBJECT, "Created directory %s to write certs to", path.c_str());
        }
    } catch (std::exception &e) {
        LOG_E(CERT_UPDATER_SUBJECT, "Failed to create write directory %s for certs: %s", path.c_str(), e.what());
        return -2;
    }

    try {
        auto out_key_path = std::ofstream(path / EMQX_KEY_PATH);
        out_key_path << privateKeyValue.c_str();
        out_key_path.close();

        auto out_pem_path = std::ofstream(path / EMQX_PEM_PATH);
        out_pem_path << certValue.c_str();
        out_pem_path.close();

        // TODO: ensure this chain order matches https://www.rfc-editor.org/rfc/rfc4346#section-7.4.2
        auto out_ca_path = std::ofstream(path / EMQX_CA_PATH);
        for (const auto &str : allCAsValue) {
            out_ca_path << str << std::endl;
        }
        out_ca_path.close();

    } catch (std::exception &e) {
        // TODO: unit test for this branch
        LOG_E(CERT_UPDATER_SUBJECT, "Could not write certs to directory: %s", e.what());
        return -3;
    }

    return 0;
}

void CertificateUpdatesHandler::OnStreamEvent(GG::CertificateUpdateEvent *response) {

    LOG_I(CERT_UPDATER_SUBJECT, "Retrieving all certs...");
    if (!response) {
        LOG_E(CERT_UPDATER_SUBJECT, "Received CertificateUpdateEvent response was null");
        return;
    }

    auto certUpdate = response->GetCertificateUpdate();
    if (!certUpdate) {
        LOG_E(CERT_UPDATER_SUBJECT, "Failed to get certificate update");
        return;
    }

    auto privateKey = certUpdate->GetPrivateKey();
    if (!privateKey || privateKey->empty()) {
        LOG_E(CERT_UPDATER_SUBJECT, "Failed to get private key");
        return;
    }

    auto cert = certUpdate->GetCertificate();
    if (!cert || cert->empty()) {
        LOG_E(CERT_UPDATER_SUBJECT, "Failed to get cert");
        return;
    }

    auto allCAs = certUpdate->GetCaCertificates();
    if (!allCAs || allCAs->empty()) {
        LOG_E(CERT_UPDATER_SUBJECT, "Failed to get CA certs");
        return;
    }
    int writeStatus = writeCertsToFiles(privateKey.value(), cert.value(), allCAs.value());
    if (writeStatus != 0) {
        LOG_E(CERT_UPDATER_SUBJECT, "Failed to write certificates to files with code %d", writeStatus);
    } else {
        LOG_I(CERT_UPDATER_SUBJECT, "Updated all certs!");
    }

    if (subscription) {
        subscription->operator()(response);
    }
}

bool CertificateUpdatesHandler::OnStreamError(OperationError *error) {
    if (error != nullptr) {
        LOG_E(CERT_UPDATER_SUBJECT, "OnStream error %s", error->GetMessage().value().c_str());
    }
    return false; // Return true to close stream, false to keep stream open.
}

void CertificateUpdatesHandler::OnStreamClosed() { LOG_I(CERT_UPDATER_SUBJECT, "Stream closed"); }
