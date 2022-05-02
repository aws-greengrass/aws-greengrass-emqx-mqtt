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
            };

            TEST_F(CDAIntegrationTester, CDAIntegrationInitTest) {
                EXPECT_EQ(7 * 6, 42);
            }
        }
    }
}