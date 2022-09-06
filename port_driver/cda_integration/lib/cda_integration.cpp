/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <aws/greengrass/GreengrassCoreIpcClient.h>
#include <filesystem>
#include <functional>

#include "cda_integration.h"

#define DEFAULT_TIMEOUT_SECONDS 5.0

const char *ClientDeviceAuthIntegration::FAILED_OPERATION_FMT = "Failed creating %s";
const char *ClientDeviceAuthIntegration::FAILED_ACTIVATION_FMT = "%s failed to activate with error %s";
const char *ClientDeviceAuthIntegration::FAILED_TIMEOUT_ERROR_FMT =
    "%s timed out while waiting for response from Greengrass Core";
const char *ClientDeviceAuthIntegration::FAILED_RESPONSE_TYPE_FMT = "%s failed with response type %d";
const char *ClientDeviceAuthIntegration::FAILED_RESPONSE_MESSAGE_FMT = "%s failure response: %s";
const char *ClientDeviceAuthIntegration::FAILED_RPC_ERROR_FMT = "RPC error during %s";
const char *ClientDeviceAuthIntegration::FAILED_NO_RESPONSE_VALUE = "%s response does not have a value";

const char *ClientDeviceAuthIntegration::GET_CLIENT_DEVICE_AUTH_TOKEN_OP = "GetClientDeviceAuthToken";
const char *ClientDeviceAuthIntegration::AUTHORIZE_CLIENT_DEVICE_ACTION = "AuthorizeClientDeviceAction";
const char *ClientDeviceAuthIntegration::VERIFY_CLIENT_DEVICE_IDENTITY = "VerifyClientDeviceIdentity";

const char *ClientDeviceAuthIntegration::INVALID_AUTH_TOKEN_ERROR = "aws.greengrass#InvalidClientDeviceAuthTokenError";

const char *IPC_TIMEOUT_SECONDS_ENV_VAR = "IPC_TIMEOUT_SECONDS";

void ClientDeviceAuthIntegration::connect() {
    greengrassIpcWrapper.connect();
    greengrassIpcWrapper.setAsRunning();
}

GreengrassIPCWrapper &ClientDeviceAuthIntegration::getIPCWrapper() { return greengrassIpcWrapper; }

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
        LOG_E(CDA_INTEG_SUBJECT, FAILED_OPERATION_FMT, GET_CLIENT_DEVICE_AUTH_TOKEN_OP);
        return {};
    }

    auto activate = operation->Activate(request).get();
    if (!activate) {
        LOG_E(CDA_INTEG_SUBJECT, FAILED_ACTIVATION_FMT, GET_CLIENT_DEVICE_AUTH_TOKEN_OP,
              activate.StatusToString().c_str());
        return {};
    }

    auto responseFuture = operation->GetOperationResult();
    if (responseFuture.wait_for(std::chrono::seconds(timeoutSeconds)) == std::future_status::timeout) {
        LOG_E(CDA_INTEG_SUBJECT, FAILED_TIMEOUT_ERROR_FMT, GET_CLIENT_DEVICE_AUTH_TOKEN_OP);
        return {};
    }

    auto response = GG::GetClientDeviceAuthTokenResult(responseFuture.get());
    auto responseType = response.GetResultType();
    if (responseType != OPERATION_RESPONSE) {
        handle_response_error(GET_CLIENT_DEVICE_AUTH_TOKEN_OP, responseType, response.GetOperationError());
        return {};
    }

    auto token = response.GetOperationResponse()->GetClientDeviceAuthToken();
    if (!token.has_value()) {
        LOG_E(CDA_INTEG_SUBJECT, FAILED_NO_RESPONSE_VALUE, GET_CLIENT_DEVICE_AUTH_TOKEN_OP);
        return {};
    }
    return std::make_unique<std::string>(token.value());
}

AuthorizationStatus ClientDeviceAuthIntegration::on_check_acl(const char *clientId, const char *token,
                                                              const char *resource, const char *operation) {

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
        return AuthorizationStatus::UNKNOWN_ERROR;
    }

    auto activate = authorizationOperation->Activate(request).get();
    if (!activate) {
        LOG_E(CDA_INTEG_SUBJECT, FAILED_ACTIVATION_FMT, AUTHORIZE_CLIENT_DEVICE_ACTION,
              activate.StatusToString().c_str());
        return AuthorizationStatus::UNKNOWN_ERROR;
    }

    auto responseFuture = authorizationOperation->GetOperationResult();
    if (responseFuture.wait_for(std::chrono::seconds(timeoutSeconds)) == std::future_status::timeout) {
        LOG_E(CDA_INTEG_SUBJECT, FAILED_TIMEOUT_ERROR_FMT, AUTHORIZE_CLIENT_DEVICE_ACTION);
        return AuthorizationStatus::UNKNOWN_ERROR;
    }

    auto response = GG::AuthorizeClientDeviceActionResult(responseFuture.get());
    auto responseType = response.GetResultType();

    if (responseType != OPERATION_RESPONSE) {
        auto *error = response.GetOperationError();
        handle_response_error(AUTHORIZE_CLIENT_DEVICE_ACTION, responseType, error);
        if (error != nullptr && strcmp(INVALID_AUTH_TOKEN_ERROR, error->GetModelName().c_str()) == 0) {
            return AuthorizationStatus::BAD_AUTH_TOKEN;
        }
        return AuthorizationStatus::UNKNOWN_ERROR;
    }

    auto isAllowed = response.GetOperationResponse()->GetIsAuthorized();
    if (!isAllowed.has_value()) {
        LOG_E(CDA_INTEG_SUBJECT, FAILED_NO_RESPONSE_VALUE, AUTHORIZE_CLIENT_DEVICE_ACTION);
        return AuthorizationStatus::UNKNOWN_ERROR;
    }
    return isAllowed.value() ? AuthorizationStatus::AUTHORIZED : AuthorizationStatus::UNAUTHORIZED;
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

    auto responseFuture = operation->GetOperationResult();
    if (responseFuture.wait_for(std::chrono::seconds(timeoutSeconds)) == std::future_status::timeout) {
        LOG_E(CDA_INTEG_SUBJECT, FAILED_TIMEOUT_ERROR_FMT, VERIFY_CLIENT_DEVICE_IDENTITY);
        return false;
    }

    auto response = GG::VerifyClientDeviceIdentityResult(responseFuture.get());
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
        double timeoutSeconds = DEFAULT_TIMEOUT_SECONDS;
        const char *timeoutSecondsC = std::getenv(IPC_TIMEOUT_SECONDS_ENV_VAR);
        if (timeoutSecondsC == nullptr) {
            LOG_W(CDA_INTEG_SUBJECT, "Environment variable %s was not set. Falling back to %f",
                  IPC_TIMEOUT_SECONDS_ENV_VAR, timeoutSeconds);
        } else {
            try {
                auto timeoutSecondsStr = std::string{timeoutSecondsC};
                // Parse as a double even though we want it to be an int due to gson conversion from int to double
                timeoutSeconds = std::stod(timeoutSecondsStr);
            } catch (std::exception &e) {
                LOG_W(CDA_INTEG_SUBJECT, "Failed to parse timeout value %s as a double. Falling back to %f",
                      timeoutSecondsC, timeoutSeconds);
            }
        }
        cda_integ = new ClientDeviceAuthIntegration(client, static_cast<int>(timeoutSeconds));
    } catch (std::exception &e) {
        LOG_E(CDA_INTEG_SUBJECT, "Failed to initialize CDA integration %s", e.what());
    } catch (...) {
        LOG_E(CDA_INTEG_SUBJECT, "Failed to initialize CDA integration due to an unknown error");
    }
    return cda_integ;
}

ClientDeviceAuthIntegration *cda_integration_init() { return cda_integration_init(nullptr); }

void cda_integration_close(ClientDeviceAuthIntegration *cda_integ) { delete cda_integ; }

void ClientDeviceAuthIntegration::handle_response_error(const char *action,
                                                        const Aws::Eventstreamrpc::ResultType &responseType,
                                                        Aws::Eventstreamrpc::OperationError *error) {
    LOG_E(CDA_INTEG_SUBJECT, FAILED_RESPONSE_TYPE_FMT, action, responseType);
    if (responseType == OPERATION_ERROR) {
        if (error != nullptr) {
            LOG_E(CDA_INTEG_SUBJECT, FAILED_RESPONSE_MESSAGE_FMT, action, error->GetMessage().value().c_str());
        }
    } else {
        LOG_E(CDA_INTEG_SUBJECT, FAILED_RPC_ERROR_FMT, action);
    }
}
