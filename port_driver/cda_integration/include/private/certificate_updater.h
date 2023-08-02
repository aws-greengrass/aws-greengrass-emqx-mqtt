/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#pragma once

#include <atomic>
#include <aws/greengrass/GreengrassCoreIpcClient.h>
#include <filesystem>

namespace GG = Aws::Greengrass;

enum class CertWriteStatus : int {
    WRITE_SUCCESS = 0,
    WRITE_ERROR_BASE_PATH = -1,
    WRITE_ERROR_DIR_PATH = -2,
    WRITE_ERROR_OUT_STREAM = -3
};

enum class CertSubscribeUpdateStatus : int {
    SUBSCRIBE_SUCCESS = 0,
    SUBSCRIBE_ERROR_CREATE_OPERATION = -1,
    SUBSCRIBE_ERROR_TIMEOUT_RESPONSE = -2,
    SUBSCRIBE_ERROR_FAILURE_RESPONSE = -3
};

class CertificateUpdatesHandler : public GG::SubscribeToCertificateUpdatesStreamHandler {
    bool OnStreamError(OperationError *error) override;
    void OnStreamClosed() override;

  public:
    virtual ~CertificateUpdatesHandler() = default;

    CertificateUpdatesHandler(std::unique_ptr<std::filesystem::path> basePath,
                              std::unique_ptr<std::function<void(GG::CertificateUpdateEvent *)>> subscription)
        : basePath(std::move(basePath)), subscription(std::move(subscription)) {}

    // TODO: move these out of public
    void OnStreamEvent(GG::CertificateUpdateEvent *response) override;
    CertWriteStatus writeCertsToFiles(const Aws::Crt::String &privateKeyValue, const Aws::Crt::String &certValue,
                                      const Aws::Crt::Vector<Aws::Crt::String> &caCerts);

  private:
    const std::unique_ptr<std::filesystem::path> basePath;
    const std::unique_ptr<std::function<void(GG::CertificateUpdateEvent *)>> subscription;
};

class CertificateUpdater {
  private:
    GG::GreengrassCoreIpcClient &ipcClient;
    std::shared_ptr<CertificateUpdatesHandler> updatesHandler;
    std::shared_ptr<GG::SubscribeToCertificateUpdatesOperation> operation;
    // for subscription idempotency
    bool subscribed = false;
    // for simulating an unsubscribe operation, since one doesn't exist in IPC currently
    const std::shared_ptr<std::atomic<bool>> subscriptionActive = std::make_shared<std::atomic<bool>>(true);

  public:
    CertificateUpdater(GG::GreengrassCoreIpcClient &client) : ipcClient(client), updatesHandler({}){};

    CertSubscribeUpdateStatus
    subscribeToUpdates(std::unique_ptr<std::filesystem::path> basePath,
                       std::unique_ptr<std::function<void(GG::CertificateUpdateEvent *)>> subscription);

    void unsubscribeFromUpdates();
};
