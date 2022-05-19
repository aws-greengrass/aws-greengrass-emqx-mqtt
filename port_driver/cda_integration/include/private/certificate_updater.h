/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#pragma once

#include "certificate_updates_handler.h"
#include <aws/greengrass/GreengrassCoreIpcClient.h>

namespace GG = Aws::Greengrass;

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
