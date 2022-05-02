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
            private:
                static const std::string TEST_CLIENT_ID;
                static const std::string TEST_CLIENT_PEM;

            protected:
                CDAIntegrationTester() {
                }
            };

            const std::string CDAIntegrationTester::TEST_CLIENT_ID = "test_client_id";
            const std::string CDAIntegrationTester::TEST_CLIENT_PEM = "test_client_pem";

            TEST_F(CDAIntegrationTester, CDAIntegrationInitAndCloseTest) {
                CDA_INTEGRATION_HANDLE* handle = cda_integration_init();
                EXPECT_NE(nullptr, handle);
                EXPECT_TRUE(cda_integration_close(handle));
            }

            TEST_F(CDAIntegrationTester, CDAIntegrationOnClientConnectTest) {
                CDA_INTEGRATION_HANDLE* handle = cda_integration_init();
                EXPECT_TRUE(on_client_connect(handle, test_client_id.c_str(), test_client_pem.c_str()));
                EXPECT_TRUE(cda_integration_close(handle));
            }
        }
    }
}
