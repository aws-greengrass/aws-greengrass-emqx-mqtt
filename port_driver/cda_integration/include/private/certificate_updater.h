/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#pragma once

#include <aws/greengrass/GreengrassCoreIpcClient.h>

namespace GG = Aws::Greengrass;

class CertificateUpdatesHandler : public GG::SubscribeToCertificateUpdatesStreamHandler {
    void OnStreamEvent(GG::CertificateUpdateEvent *response) override;
    bool OnStreamError(OperationError *error) override;
    void OnStreamClosed() override;

  public:
    virtual ~CertificateUpdatesHandler() = default;

    CertificateUpdatesHandler(std::unique_ptr<std::filesystem::path> basePath,
                              std::unique_ptr<std::function<void(GG::CertificateUpdateEvent *)>> subscription)
        : basePath(std::move(basePath)), subscription(std::move(subscription)) {}

  private:
    int writeCertsToFiles(Aws::Crt::String &privateKeyValue, Aws::Crt::String &certValue,
                          std::vector<Aws::Crt::String, Aws::Crt::StlAllocator<Aws::Crt::String>> &);

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
    int subscribeToUpdates(std::unique_ptr<std::filesystem::path> basePath,
                           std::unique_ptr<std::function<void(GG::CertificateUpdateEvent *)>> subscription);
};
