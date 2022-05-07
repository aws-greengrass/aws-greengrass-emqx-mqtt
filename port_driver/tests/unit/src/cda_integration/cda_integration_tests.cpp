/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <gtest/gtest.h>

#include <aws/greengrass/GreengrassCoreIpcClient.h>

#include "cda_integration.h"
#include "mock_ipc.hpp"

using namespace Aws::Crt;
using namespace Aws::Greengrass;

namespace port_driver {
namespace tests {
namespace unit {
class CDAIntegrationTester : public ::testing::Test {
  protected:
    CDAIntegrationTester() = default;

    static const std::string TEST_CLIENT_ID;
    static const std::string TEST_CLIENT_PEM;
    static const std::string TEST_TOPIC;
    static const std::string TEST_ACTION;

    virtual void SetUp();
    virtual void TearDown();

    CDA_INTEGRATION_HANDLE *handle;
};

const std::string CDAIntegrationTester::TEST_CLIENT_ID = "test_client_id";
const std::string CDAIntegrationTester::TEST_CLIENT_PEM = "test_client_pem";
const std::string CDAIntegrationTester::TEST_TOPIC = "/topic";
const std::string CDAIntegrationTester::TEST_ACTION = "publish";

void CDAIntegrationTester::SetUp() {
    std::unique_ptr<GreengrassCoreIpcClient> client = std::make_unique<MockGGIpc>();
    handle = cda_integration_init(std::move(client));
    EXPECT_NE(nullptr, handle);
}

void CDAIntegrationTester::TearDown() {
    if (handle == nullptr) {
        EXPECT_FALSE(cda_integration_close(handle));
    } else {
        EXPECT_TRUE(cda_integration_close(handle));
    }
}

TEST_F(CDAIntegrationTester, CDAIntegrationInitAndCloseTest) { EXPECT_NE(nullptr, handle); }

TEST_F(CDAIntegrationTester, CDAIntegrationOnClientConnectTest) {
    EXPECT_TRUE(on_client_connect(handle, TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str()));
}

TEST_F(CDAIntegrationTester, CDAIntegrationOnClientConnectedTest) {
    EXPECT_TRUE(on_client_connected(handle, TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str()));
}

TEST_F(CDAIntegrationTester, CDAIntegrationOnClientDisconnectTest) {
    EXPECT_TRUE(on_client_disconnected(handle, TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str()));
}

TEST_F(CDAIntegrationTester, CDAIntegrationOnClientAuthenticateTest) {
    EXPECT_TRUE(on_client_authenticate(handle, TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str()));
}

TEST_F(CDAIntegrationTester, CDAIntegrationOnCheckAclTest) {
    EXPECT_TRUE(
        on_check_acl(handle, TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str(), TEST_TOPIC.c_str(), TEST_ACTION.c_str()));
}

TEST_F(CDAIntegrationTester, CDAIntegrationNullHandleTest) {
    EXPECT_FALSE(on_check_acl(nullptr, TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str(), TEST_TOPIC.c_str(),
                              TEST_ACTION.c_str()));
}
} // namespace unit
} // namespace tests
} // namespace port_driver
