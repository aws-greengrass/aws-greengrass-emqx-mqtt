/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#pragma once

#include <aws/greengrass/GreengrassCoreIpcClient.h>

namespace GG = Aws::Greengrass;

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
    int writeCertsToFiles(Aws::Crt::String &privateKeyValue, Aws::Crt::String &certValue,
                          std::vector<Aws::Crt::String, Aws::Crt::StlAllocator<Aws::Crt::String>> &);

  private:
    const std::unique_ptr<std::filesystem::path> basePath;
    const std::unique_ptr<std::function<void(GG::CertificateUpdateEvent *)>> subscription;
};
