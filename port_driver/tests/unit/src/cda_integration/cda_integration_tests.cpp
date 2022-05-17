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

    ClientDeviceAuthIntegration *cda_integ;
};

const std::string CDAIntegrationTester::TEST_CLIENT_ID = "test_client_id";
const std::string CDAIntegrationTester::TEST_CLIENT_PEM = "test_client_pem";
const std::string CDAIntegrationTester::TEST_TOPIC = "/topic";
const std::string CDAIntegrationTester::TEST_ACTION = "publish";

void CDAIntegrationTester::SetUp() {
    GreengrassCoreIpcClient *ipcClient = new MockGGIpc();
    cda_integ = cda_integration_init(ipcClient);
    EXPECT_NE(nullptr, cda_integ);
}

void CDAIntegrationTester::TearDown() { cda_integration_close(cda_integ); }

TEST_F(CDAIntegrationTester, CDAIntegrationOnClientConnectTest) {
    EXPECT_TRUE(cda_integ->on_client_connect(TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str()));
}

TEST_F(CDAIntegrationTester, CDAIntegrationOnClientConnectedTest) {
    EXPECT_TRUE(cda_integ->on_client_connected(TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str()));
}

TEST_F(CDAIntegrationTester, CDAIntegrationOnClientDisconnectTest) {
    EXPECT_TRUE(cda_integ->on_client_disconnected(TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str()));
}

TEST_F(CDAIntegrationTester, CDAIntegrationOnClientAuthenticateTest) {
    auto response = cda_integ->get_client_device_auth_token(TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str());
    std::string expected = "";
    EXPECT_EQ(expected, response.get()->c_str());
}

TEST_F(CDAIntegrationTester, CDAIntegrationOnCheckAclTest) {
    EXPECT_TRUE(cda_integ->on_check_acl(TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str(), TEST_TOPIC.c_str(),
                                        TEST_ACTION.c_str()));
}

} // namespace unit
} // namespace tests
} // namespace port_driver
