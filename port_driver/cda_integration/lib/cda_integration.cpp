/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <aws/greengrass/GreengrassCoreIpcClient.h>
#include <filesystem>
#include <functional>
#include <iostream>

#include "cda_integration.h"

#define TIMEOUT_SECONDS 2

void ClientDeviceAuthIntegration::connect() {
    greengrassIpcWrapper.connect();
    greengrassIpcWrapper.setAsRunning();
}

int ClientDeviceAuthIntegration::subscribeToCertUpdates(
    std::unique_ptr<std::filesystem::path> basePath,
    std::unique_ptr<std::function<void(GG::CertificateUpdateEvent *)>> subscription) {
    int certSubscribeStatus = certificateUpdater.subscribeToUpdates(std::move(basePath), std::move(subscription));
    if (certSubscribeStatus != 0) {
        LOG_E(CDA_INTEG_SUBJECT, "Failed to subscribe to cert updates with status %d", certSubscribeStatus);
        return certSubscribeStatus;
    }
    LOG_I(CDA_INTEG_SUBJECT, "Certs were successfully retrieved from Greengrass IPC");
    return 0;
}

// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
bool ClientDeviceAuthIntegration::on_client_connect(const char *clientId, const char *pem) {
    std::cout << "on_client_connect called with clientId: " << clientId << " and pem: " << pem << std::endl;
    return true;
}

// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
bool ClientDeviceAuthIntegration::on_client_connected(const char *clientId, const char *pem) {
    std::cout << "on_client_connected called with clientId: " << clientId << " and pem: " << pem << std::endl;
    return true;
}

// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
bool ClientDeviceAuthIntegration::on_client_disconnected(const char *clientId, const char *pem) {
    std::cout << "on_client_disconnected called with clientId: " << clientId << " and pem: " << pem << std::endl;
    return true;
}

std::unique_ptr<std::string> ClientDeviceAuthIntegration::get_client_device_auth_token(const char *clientId,
                                                                                       const char *pem) {
    LOG_D(CDA_INTEG_SUBJECT, "on_client_authenticate called with clientId: %s", clientId);
    Aws::Crt::String clientIdStr(clientId);
    Aws::Crt::String pemStr(pem);

    GG::MQTTCredential mqttCredential;
    mqttCredential.SetClientId(clientIdStr);
    mqttCredential.SetCertificatePem(pemStr);

    GG::CredentialDocument credentialDocument;
    credentialDocument.SetMqttCredential(mqttCredential);

    GG::GetClientDeviceAuthTokenRequest request;
    request.SetCredential(credentialDocument);

    auto operation = greengrassIpcWrapper.getIPCClient().NewGetClientDeviceAuthToken();
    if (!operation) {
        LOG_E(CDA_INTEG_SUBJECT, "Failed creating NewGetClientDeviceAuthToken.");
        return {};
    }

    auto activate = operation->Activate(request).get();
    if (!activate) {
        LOG_E(CDA_INTEG_SUBJECT, "NewGetClientDeviceAuthToken failed to activate with error %s",
              activate.StatusToString().c_str());
        return {};
    }

    auto responseFuture = operation->GetResult();
    if (responseFuture.wait_for(std::chrono::seconds(TIMEOUT_SECONDS)) == std::future_status::timeout) {
        LOG_E(CDA_INTEG_SUBJECT,
              "NewGetClientDeviceAuthToken timed out while waiting for response from Greengrass Core.");
        return {};
    }

    auto response = responseFuture.get();
    auto responseType = response.GetResultType();
    if (responseType != OPERATION_RESPONSE) {
        // Handle error.
        LOG_E(CDA_INTEG_SUBJECT, "NewGetClientDeviceAuthToken failed with response type %d.", responseType);
        if (responseType == OPERATION_ERROR) {
            auto *error = response.GetOperationError();
            if (error != nullptr) {
                LOG_E(CDA_INTEG_SUBJECT, "NewGetClientDeviceAuthToken failure response: %s.",
                      error->GetMessage().value().c_str());
            }
        } else {
            LOG_E(CDA_INTEG_SUBJECT, "RPC error during NewGetClientDeviceAuthToken");
        }
        return {};
    }

    auto token = response.GetOperationResponse()->GetClientDeviceAuthToken();
    if (!token.has_value()) {
        LOG_E(CDA_INTEG_SUBJECT, "Token received from CDA does not have a value.");
        return {};
    }
    return std::make_unique<std::string>(token.value());
}

// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
bool ClientDeviceAuthIntegration::on_check_acl(const char *clientId, const char *pem, const char *topic,
                                               const char *action) {
    std::cout << "on_check_acl called with clientId: " << clientId << " and pem: " << pem << " and topic: " << topic
              << " and action: " << action << std::endl;
    return true;
}

// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
bool ClientDeviceAuthIntegration::verify_client_certificate(const char *certPem) {
    LOG_D(CDA_INTEG_SUBJECT, "verify_client_certificate called");
    Aws::Crt::String certPemStr(certPem);

    GG::ClientDeviceCredential clientDeviceCredential;
    clientDeviceCredential.SetClientDeviceCertificate(certPemStr);

    GG::VerifyClientDeviceIdentityRequest request;
    request.SetCredential(clientDeviceCredential);

    auto operation = greengrassIpcWrapper.getIPCClient().NewVerifyClientDeviceIdentity();
    if (!operation) {
        LOG_E(CDA_INTEG_SUBJECT, "Failed creating NewVerifyClientDeviceIdentity.");
        return false;
    }

    auto activate = operation->Activate(request).get();

    if (!activate) {
        LOG_E(CDA_INTEG_SUBJECT, "VerifyClientDeviceIdentity failed to activate with error %s",
              activate.StatusToString().c_str());
        return false;
    }

    auto responseFuture = operation->GetResult();
    if (responseFuture.wait_for(std::chrono::seconds(TIMEOUT_SECONDS)) == std::future_status::timeout) {
        LOG_E(CDA_INTEG_SUBJECT,
              "VerifyClientDeviceIdentity operation timed out while waiting for response from Greengrass Core.");
        return false;
    }

    auto response = responseFuture.get();
    auto responseType = response.GetResultType();
    if (responseType != OPERATION_RESPONSE) {
        // Handle error.
        LOG_E(CDA_INTEG_SUBJECT, "VerifyClientDeviceIdentity failed with response type %d.", responseType);
        if (responseType == OPERATION_ERROR) {
            auto *error = response.GetOperationError();
            if (error != nullptr) {
                LOG_E(CDA_INTEG_SUBJECT, "VerifyClientDeviceIdentity failure response: %s.",
                      error->GetMessage().value().c_str());
            }
        } else {
            LOG_E(CDA_INTEG_SUBJECT, "RPC error during VerifyClientDeviceIdentity");
        }
        return false;
    }

    auto isValid = response.GetOperationResponse()->GetIsValidClientDevice();
    if (!isValid.has_value()) {
        LOG_E(CDA_INTEG_SUBJECT, "VerifyClientDeviceIdentityResponse does not have a value.");
        return false;
    }
    return isValid.value();
}

ClientDeviceAuthIntegration *cda_integration_init(GG::GreengrassCoreIpcClient *client) {
    ClientDeviceAuthIntegration *cda_integ = nullptr;
    try {
        cda_integ = new ClientDeviceAuthIntegration(client);
    } catch (std::exception &e) {
        LOG_E(CDA_INTEG_SUBJECT, "Failed to initialize CDA integration %s", e.what());
    } catch (...) {
        LOG_E(CDA_INTEG_SUBJECT, "Failed to initialize CDA integration due to an unknown error");
    }
    return cda_integ;
}

ClientDeviceAuthIntegration *cda_integration_init() { return cda_integration_init(nullptr); }

void cda_integration_close(ClientDeviceAuthIntegration *cda_integ) { delete cda_integ; }
