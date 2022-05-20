/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#pragma once

#include <aws/greengrass/GreengrassCoreIpcClient.h>
#include <filesystem>

namespace GG = Aws::Greengrass;

enum class CertWriteStatus { WRITE_SUCCESS = 0, WRITE_ERROR_BASE_PATH, WRITE_ERROR_DIR_PATH, WRITE_ERROR_OUT_STREAM };

enum class CertSubscribeUpdateStatus {
    SUBSCRIBE_SUCCESS = 0,
    SUBSCRIBE_ERROR_CREATE_OPERATION,
    SUBSCRIBE_ERROR_TIMEOUT_RESPONSE,
    SUBSCRIBE_ERROR_FAILURE_RESPONSE
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
    CertWriteStatus writeCertsToFiles(Aws::Crt::String &privateKeyValue, Aws::Crt::String &certValue,
                                      std::vector<Aws::Crt::String, Aws::Crt::StlAllocator<Aws::Crt::String>> &);

  private:
    const std::unique_ptr<std::filesystem::path> basePath;
    const std::unique_ptr<std::function<void(GG::CertificateUpdateEvent *)>> subscription;
};

class CertificateUpdater {
  private:
    GG::GreengrassCoreIpcClient &ipcClient;
    std::shared_ptr<CertificateUpdatesHandler> updatesHandler;
    std::shared_ptr<GG::SubscribeToCertificateUpdatesOperation> operation;

  public:
    CertificateUpdater(GG::GreengrassCoreIpcClient &client) : ipcClient(client), updatesHandler({}){};

    // TODO: Improve return codes
    CertSubscribeUpdateStatus
    subscribeToUpdates(std::unique_ptr<std::filesystem::path> basePath,
                       std::unique_ptr<std::function<void(GG::CertificateUpdateEvent *)>> subscription);
};
