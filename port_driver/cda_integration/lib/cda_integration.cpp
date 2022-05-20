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

#define FAILED_OPERATION_FMT "Failed creating %s"
#define FAILED_ACTIVATION_FMT "%s failed to activate with error %s"
#define FAILED_TIMEOUT_ERROR_FMT "%s timed out while waiting for response from Greengrass Core"
#define FAILED_RESPONSE_TYPE_FMT "%s failed with response type %d"
#define FAILED_RESPONSE_MESSAGE_FMT "%s failure response: %s"
#define FAILED_RPC_ERROR_FMT "RPC error during %s"
#define FAILED_NO_RESPONSE_VALUE "%s response does not have a value"

#define GET_CLIENT_DEVICE_AUTH_TOKEN "GetClientDeviceAuthToken"
#define AUTHORIZE_CLIENT_DEVICE_ACTION "AuthorizeClientDeviceAction"
#define VERIFY_CLIENT_DEVICE_IDENTITY "VerifyClientDeviceIdentity"

void ClientDeviceAuthIntegration::connect() {
    greengrassIpcWrapper.connect();
    greengrassIpcWrapper.setAsRunning();
}

CertSubscribeUpdateStatus ClientDeviceAuthIntegration::subscribeToCertUpdates(
    std::unique_ptr<std::filesystem::path> basePath,
    std::unique_ptr<std::function<void(GG::CertificateUpdateEvent *)>> subscription) {
    CertSubscribeUpdateStatus certSubscribeStatus =
        certificateUpdater.subscribeToUpdates(std::move(basePath), std::move(subscription));
    if (certSubscribeStatus != CertSubscribeUpdateStatus::SUBSCRIBE_SUCCESS) {
        LOG_E(CDA_INTEG_SUBJECT, "Failed to subscribe to cert updates with status %d", (int)certSubscribeStatus);
        return certSubscribeStatus;
    }
    LOG_I(CDA_INTEG_SUBJECT, "Certs were successfully retrieved from Greengrass IPC");
    return CertSubscribeUpdateStatus::SUBSCRIBE_SUCCESS;
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
    LOG_D(CDA_INTEG_SUBJECT, "get_client_device_auth_token called with clientId: %s", clientId);
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
        LOG_E(CDA_INTEG_SUBJECT, FAILED_OPERATION_FMT, GET_CLIENT_DEVICE_AUTH_TOKEN);
        return {};
    }

    auto activate = operation->Activate(request).get();
    if (!activate) {
        LOG_E(CDA_INTEG_SUBJECT, FAILED_ACTIVATION_FMT, GET_CLIENT_DEVICE_AUTH_TOKEN,
              activate.StatusToString().c_str());
        return {};
    }

    auto responseFuture = operation->GetResult();
    if (responseFuture.wait_for(std::chrono::seconds(TIMEOUT_SECONDS)) == std::future_status::timeout) {
        LOG_E(CDA_INTEG_SUBJECT, FAILED_TIMEOUT_ERROR_FMT, GET_CLIENT_DEVICE_AUTH_TOKEN);
        return {};
    }

    auto response = responseFuture.get();
    auto responseType = response.GetResultType();
    if (responseType != OPERATION_RESPONSE) {
        handle_response_error(GET_CLIENT_DEVICE_AUTH_TOKEN, responseType, response.GetOperationError());
        return {};
    }

    auto token = response.GetOperationResponse()->GetClientDeviceAuthToken();
    if (!token.has_value()) {
        LOG_E(CDA_INTEG_SUBJECT, FAILED_NO_RESPONSE_VALUE, GET_CLIENT_DEVICE_AUTH_TOKEN);
        return {};
    }
    return std::make_unique<std::string>(token.value());
}

bool ClientDeviceAuthIntegration::on_check_acl(const char *clientId, const char *token, const char *resource,
                                               const char *operation) {

    LOG_D(CDA_INTEG_SUBJECT, "on_check_acl called with clientId: %s, token: %s, resource: %s, operation: %s", clientId,
          token, resource, operation);

    Aws::Crt::String tokenStr(token);
    Aws::Crt::String resourceStr(resource);
    Aws::Crt::String operationStr(operation);

    GG::AuthorizeClientDeviceActionRequest request;
    request.SetClientDeviceAuthToken(tokenStr);
    request.SetOperation(operationStr);
    request.SetResource(resourceStr);

    auto authorizationOperation = greengrassIpcWrapper.getIPCClient().NewAuthorizeClientDeviceAction();
    if (!authorizationOperation) {
        LOG_E(CDA_INTEG_SUBJECT, FAILED_OPERATION_FMT, AUTHORIZE_CLIENT_DEVICE_ACTION);
        return false;
    }

    auto activate = authorizationOperation->Activate(request).get();
    if (!activate) {
        LOG_E(CDA_INTEG_SUBJECT, FAILED_ACTIVATION_FMT, AUTHORIZE_CLIENT_DEVICE_ACTION,
              activate.StatusToString().c_str());
        return false;
    }

    auto responseFuture = authorizationOperation->GetResult();
    if (responseFuture.wait_for(std::chrono::seconds(TIMEOUT_SECONDS)) == std::future_status::timeout) {
        LOG_E(CDA_INTEG_SUBJECT, FAILED_TIMEOUT_ERROR_FMT, AUTHORIZE_CLIENT_DEVICE_ACTION);
        return false;
    }

    auto response = responseFuture.get();
    auto responseType = response.GetResultType();

    if (responseType != OPERATION_RESPONSE) {
        handle_response_error(AUTHORIZE_CLIENT_DEVICE_ACTION, responseType, response.GetOperationError());
        return false;
    }

    auto isAllowed = response.GetOperationResponse()->GetIsAuthorized();
    if (!isAllowed.has_value()) {
        LOG_E(CDA_INTEG_SUBJECT, FAILED_NO_RESPONSE_VALUE, AUTHORIZE_CLIENT_DEVICE_ACTION);
        return false;
    }
    return isAllowed.value();
}

bool ClientDeviceAuthIntegration::verify_client_certificate(const char *certPem) {
    LOG_D(CDA_INTEG_SUBJECT, "verify_client_certificate called");
    Aws::Crt::String certPemStr(certPem);

    GG::ClientDeviceCredential clientDeviceCredential;
    clientDeviceCredential.SetClientDeviceCertificate(certPemStr);

    GG::VerifyClientDeviceIdentityRequest request;
    request.SetCredential(clientDeviceCredential);

    auto operation = greengrassIpcWrapper.getIPCClient().NewVerifyClientDeviceIdentity();
    if (!operation) {
        LOG_E(CDA_INTEG_SUBJECT, FAILED_OPERATION_FMT, VERIFY_CLIENT_DEVICE_IDENTITY);
        return false;
    }

    auto activate = operation->Activate(request).get();

    if (!activate) {
        LOG_E(CDA_INTEG_SUBJECT, FAILED_ACTIVATION_FMT, VERIFY_CLIENT_DEVICE_IDENTITY,
              activate.StatusToString().c_str());
        return false;
    }

    auto responseFuture = operation->GetResult();
    if (responseFuture.wait_for(std::chrono::seconds(TIMEOUT_SECONDS)) == std::future_status::timeout) {
        LOG_E(CDA_INTEG_SUBJECT, FAILED_TIMEOUT_ERROR_FMT, VERIFY_CLIENT_DEVICE_IDENTITY);
        return false;
    }

    auto response = responseFuture.get();
    auto responseType = response.GetResultType();
    if (responseType != OPERATION_RESPONSE) {
        handle_response_error(VERIFY_CLIENT_DEVICE_IDENTITY, responseType, response.GetOperationError());
        return false;
    }

    auto isValid = response.GetOperationResponse()->GetIsValidClientDevice();
    if (!isValid.has_value()) {
        LOG_E(CDA_INTEG_SUBJECT, FAILED_NO_RESPONSE_VALUE, VERIFY_CLIENT_DEVICE_IDENTITY);
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

void ClientDeviceAuthIntegration::handle_response_error(const std::string &action,
                                                        const Aws::Eventstreamrpc::ResultType &responseType,
                                                        Aws::Eventstreamrpc::OperationError *error) {
    LOG_E(CDA_INTEG_SUBJECT, FAILED_RESPONSE_TYPE_FMT, action, responseType);
    if (responseType == OPERATION_ERROR) {
        if (error != nullptr) {
            LOG_E(CDA_INTEG_SUBJECT, FAILED_RESPONSE_MESSAGE_FMT, VERIFY_CLIENT_DEVICE_IDENTITY,
                  error->GetMessage().value().c_str());
        }
    } else {
        LOG_E(CDA_INTEG_SUBJECT, FAILED_RPC_ERROR_FMT, VERIFY_CLIENT_DEVICE_IDENTITY);
    }
}
