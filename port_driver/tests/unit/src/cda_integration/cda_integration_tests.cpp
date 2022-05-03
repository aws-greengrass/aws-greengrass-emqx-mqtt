/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <gtest/gtest.h>

#include "cda_integration.h"

namespace port_driver {
    namespace tests {
        namespace unit {
            class CDAIntegrationTester : public ::testing::Test {
            protected:
                CDAIntegrationTester() {
                }

                static const std::string TEST_CLIENT_ID;
                static const std::string TEST_CLIENT_PEM;
                static const std::string TEST_TOPIC;
                static const std::string TEST_ACTION;
            };

            const std::string CDAIntegrationTester::TEST_CLIENT_ID = "test_client_id";
            const std::string CDAIntegrationTester::TEST_CLIENT_PEM = "test_client_pem";
            const std::string CDAIntegrationTester::TEST_TOPIC = "/topic";
            const std::string CDAIntegrationTester::TEST_ACTION = "publish";

            TEST_F(CDAIntegrationTester, CDAIntegrationInitAndCloseTest) {
                CDA_INTEGRATION_HANDLE* handle = cda_integration_init();
                EXPECT_NE(nullptr, handle);
                EXPECT_TRUE(cda_integration_close(handle));
            }

            TEST_F(CDAIntegrationTester, CDAIntegrationOnClientConnectTest) {
                CDA_INTEGRATION_HANDLE* handle = cda_integration_init();
                EXPECT_TRUE(on_client_connect(handle, TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str()));
                EXPECT_TRUE(cda_integration_close(handle));
            }

            TEST_F(CDAIntegrationTester, CDAIntegrationOnClientConnectedTest) {
                CDA_INTEGRATION_HANDLE* handle = cda_integration_init();
                EXPECT_TRUE(on_client_connected(handle, TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str()));
                EXPECT_TRUE(cda_integration_close(handle));
            }

            TEST_F(CDAIntegrationTester, CDAIntegrationOnClientDisconnectTest) {
                CDA_INTEGRATION_HANDLE* handle = cda_integration_init();
                EXPECT_TRUE(on_client_disconnected(handle, TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str()));
                EXPECT_TRUE(cda_integration_close(handle));
            }

            TEST_F(CDAIntegrationTester, CDAIntegrationOnClientAuthenticateTest) {
                CDA_INTEGRATION_HANDLE* handle = cda_integration_init();
                EXPECT_TRUE(on_client_authenticate(handle, TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str()));
                EXPECT_TRUE(cda_integration_close(handle));
            }

            TEST_F(CDAIntegrationTester, CDAIntegrationOnCheckAclTest) {
                CDA_INTEGRATION_HANDLE* handle = cda_integration_init();
                EXPECT_TRUE(on_check_acl(handle, TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str(), TEST_TOPIC.c_str(),
                    TEST_ACTION.c_str()));
                EXPECT_TRUE(cda_integration_close(handle));
            }

            TEST_F(CDAIntegrationTester, CDAIntegrationNullHandleTest) {
                EXPECT_FALSE(on_check_acl(nullptr, TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str(), TEST_TOPIC.c_str(),
                    TEST_ACTION.c_str()));
            }
        }
    }
}
