/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <gtest/gtest.h>

#include <array>
#include <aws/greengrass/GreengrassCoreIpcClient.h>
#include <cstddef>
#include <fstream>
#include <iostream>
#include <string>

#include "cda_integration.h"
#include "test_utils.hpp"

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
    static const std::string TEST_AUTH_TOKEN;

    virtual void SetUp();
    virtual void TearDown();

    ClientDeviceAuthIntegration *cda_integ;
    FILE *pipe;
    std::ofstream writePipe;

    std::string NextCommand() {
        std::size_t bufSize = 1024;
        auto buffer = (char *)malloc(bufSize * sizeof(char));
        std::string result;
        std::string commands;
        while (getline(&buffer, &bufSize, pipe) > 0) {
            std::string r = buffer;
            rtrim(r);
            std::cout << r << std::endl;
            if (r.starts_with("EMQX: ")) {
                return r.substr(6);
            }
        }
        free(buffer);
        return {};
    }

    void SendCommand(std::string command) {
        writePipe << command << std::endl;
        if (command.starts_with("set ")) {
            EXPECT_EQ(NextCommand(), "did_set");
        }
    }
};

const std::string CDAIntegrationTester::TEST_CLIENT_ID = "test_client_id";
const std::string CDAIntegrationTester::TEST_CLIENT_PEM = "test_client_pem";
const std::string CDAIntegrationTester::TEST_TOPIC = "/topic";
const std::string CDAIntegrationTester::TEST_ACTION = "publish";
const std::string CDAIntegrationTester::TEST_AUTH_TOKEN = "test_auth_token";

void CDAIntegrationTester::SetUp() {
    aws_logger_init_standard(&our_logger, aws_default_allocator(), &logger_options);
    aws_logger_set(&our_logger);

    std::string writePath("/tmp/ggIpcWrite.sock");
    remove(writePath.c_str());
    int rc = mkfifo(writePath.c_str(), 0666);
    if (rc != 0) {
        throw std::runtime_error("mkfifo() failed");
    }

    setenv("SVCUID", "", 1);
    setenv("AWS_GG_NUCLEUS_DOMAIN_SOCKET_FILEPATH_FOR_COMPONENT", "/tmp/ggIpc.sock", 1);

    pipe = popen("cd ../../port_driver/test_support && mvn -ntp verify", "r");
    if (!pipe) {
        throw std::runtime_error("popen() failed!");
    }

    auto command = NextCommand();
    writePipe.open(writePath);
    command = NextCommand();

    cda_integ = cda_integration_init();
    EXPECT_NE(nullptr, cda_integ);

    // Wait for IPC server to be up, then try to connect, then verify that we call update_state
    command = NextCommand();
    EXPECT_EQ(command, "up");
    cda_integ->connect();
    command = NextCommand();
    EXPECT_EQ(command, "update_state");
}

void CDAIntegrationTester::TearDown() {
    writePipe.close();
    while (!NextCommand().empty()) {
    }
    pclose(pipe);
    cda_integration_close(cda_integ);
}

TEST_F(CDAIntegrationTester, CDAIntegrationOnClientAuthenticateTest) {
    auto response = cda_integ->get_client_device_auth_token(TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str());
    EXPECT_FALSE(response);
    auto command = NextCommand();
    EXPECT_EQ(command, "get_client_device_auth_token");

    SendCommand("set with_value");
    response = cda_integ->get_client_device_auth_token(TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str());
    EXPECT_TRUE(response);
    EXPECT_EQ(*response, "token");
    command = NextCommand();
    EXPECT_EQ(command, "get_client_device_auth_token");

    SendCommand("set with_error");
    response = cda_integ->get_client_device_auth_token(TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str());
    EXPECT_FALSE(response);
    command = NextCommand();
    EXPECT_EQ(command, "get_client_device_auth_token");

    SendCommand("set with_timeout");
    response = cda_integ->get_client_device_auth_token(TEST_CLIENT_ID.c_str(), TEST_CLIENT_PEM.c_str());
    EXPECT_FALSE(response);
    command = NextCommand();
    EXPECT_EQ(command, "get_client_device_auth_token");
}

TEST_F(CDAIntegrationTester, CDAIntegrationOnCheckAclTest) {
    EXPECT_EQ(AuthorizationStatus::UNKNOWN_ERROR,
              cda_integ->on_check_acl(TEST_CLIENT_ID.c_str(), TEST_AUTH_TOKEN.c_str(), TEST_TOPIC.c_str(),
                                      TEST_ACTION.c_str()));

    auto command = NextCommand();
    EXPECT_EQ(command, "authorize_client_device_action");

    SendCommand("set with_true");
    EXPECT_EQ(AuthorizationStatus::AUTHORIZED, cda_integ->on_check_acl(TEST_CLIENT_ID.c_str(), TEST_AUTH_TOKEN.c_str(),
                                                                       TEST_TOPIC.c_str(), TEST_ACTION.c_str()));
    command = NextCommand();
    EXPECT_EQ(command, "authorize_client_device_action");

    SendCommand("set with_false");
    EXPECT_EQ(AuthorizationStatus::UNAUTHORIZED,
              cda_integ->on_check_acl(TEST_CLIENT_ID.c_str(), TEST_AUTH_TOKEN.c_str(), TEST_TOPIC.c_str(),
                                      TEST_ACTION.c_str()));
    command = NextCommand();
    EXPECT_EQ(command, "authorize_client_device_action");

    SendCommand("set with_error");
    EXPECT_EQ(AuthorizationStatus::UNKNOWN_ERROR,
              cda_integ->on_check_acl(TEST_CLIENT_ID.c_str(), TEST_AUTH_TOKEN.c_str(), TEST_TOPIC.c_str(),
                                      TEST_ACTION.c_str()));
    command = NextCommand();
    EXPECT_EQ(command, "authorize_client_device_action");

    SendCommand("set with_bad_token_error");
    EXPECT_EQ(AuthorizationStatus::BAD_AUTH_TOKEN,
              cda_integ->on_check_acl(TEST_CLIENT_ID.c_str(), TEST_AUTH_TOKEN.c_str(), TEST_TOPIC.c_str(),
                                      TEST_ACTION.c_str()));
    command = NextCommand();
    EXPECT_EQ(command, "authorize_client_device_action");

    SendCommand("set with_timeout");
    EXPECT_EQ(AuthorizationStatus::UNKNOWN_ERROR,
              cda_integ->on_check_acl(TEST_CLIENT_ID.c_str(), TEST_AUTH_TOKEN.c_str(), TEST_TOPIC.c_str(),
                                      TEST_ACTION.c_str()));
    command = NextCommand();
    EXPECT_EQ(command, "authorize_client_device_action");
}

TEST_F(CDAIntegrationTester, CDAIntegrationVerifyIdentityTest) {
    EXPECT_FALSE(cda_integ->verify_client_certificate(TEST_CLIENT_PEM.c_str()));

    auto command = NextCommand();
    EXPECT_EQ(command, "verify_client_device_identity");

    SendCommand("set with_true");
    EXPECT_TRUE(cda_integ->verify_client_certificate(TEST_CLIENT_PEM.c_str()));
    command = NextCommand();
    EXPECT_EQ(command, "verify_client_device_identity");

    SendCommand("set with_false");
    EXPECT_FALSE(cda_integ->verify_client_certificate(TEST_CLIENT_PEM.c_str()));
    command = NextCommand();
    EXPECT_EQ(command, "verify_client_device_identity");

    SendCommand("set with_error");
    EXPECT_FALSE(cda_integ->verify_client_certificate(TEST_CLIENT_PEM.c_str()));
    command = NextCommand();
    EXPECT_EQ(command, "verify_client_device_identity");

    SendCommand("set with_timeout");
    EXPECT_FALSE(cda_integ->verify_client_certificate(TEST_CLIENT_PEM.c_str()));
    command = NextCommand();
    EXPECT_EQ(command, "verify_client_device_identity");
}

} // namespace unit
} // namespace tests
} // namespace port_driver
