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
    int writeCertsToFiles(Aws::Crt::String& privateKeyValue, Aws::Crt::String& certValue,
            std::vector<Aws::Crt::String, Aws::Crt::StlAllocator<Aws::Crt::String>>&);
};

class CertificateUpdater {
  private:
    GG::GreengrassCoreIpcClient &ipcClient;
    std::shared_ptr<CertificateUpdatesHandler> updatesHandler;

  public:
    CertificateUpdater(GG::GreengrassCoreIpcClient &client)
        : ipcClient(client), updatesHandler(new CertificateUpdatesHandler()){};

    // TODO: Improve return codes
    int subscribeToUpdates();
};
