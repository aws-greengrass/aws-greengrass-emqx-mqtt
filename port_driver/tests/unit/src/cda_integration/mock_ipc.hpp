/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
#pragma once

#include <gmock/gmock.h>
#include "cda_integration.h"

class MockGGIpc : public Aws::Greengrass::GreengrassCoreIpcClient {
public:
    MockGGIpc()
            : GreengrassCoreIpcClient(*Aws::Crt::ApiHandle().GetOrCreateStaticDefaultClientBootstrap(),
                                      Aws::Crt::g_allocator) {
    };

    MOCK_METHOD(
            std::shared_ptr<Aws::Greengrass::SubscribeToCertificateUpdatesOperation>,
            NewSubscribeToCertificateUpdates,
            (std::shared_ptr<Aws::Greengrass::SubscribeToCertificateUpdatesStreamHandler>),
            (noexcept)
    );
};
