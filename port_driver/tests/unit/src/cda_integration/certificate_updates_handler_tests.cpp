/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <aws/greengrass/GreengrassCoreIpcClient.h>
#include <gtest/gtest.h>
#include <iostream>

#include "private/certificate_updater.h"
#include "test_utils.hpp"

using namespace Aws::Crt;
using namespace Aws::Greengrass;

namespace port_driver {
namespace tests {
namespace unit {
class CertificateUpdatesHandlerTester : public ::testing::Test {
  public:
    CertificateUpdatesHandlerTester() = default;

    virtual void SetUp();
    virtual void TearDown();

  protected:
    std::unique_ptr<std::filesystem::path> testPath = std::make_unique<std::filesystem::path>(filePath);
    CertificateUpdatesHandler *handler;
    CertificateUpdate testCertUpdate;
    Optional<CertificateUpdate> optionalTestCertUpdate;
    CertificateUpdateEvent *testResponse;
    Vector<String> caCerts;
};

void CertificateUpdatesHandlerTester::SetUp() {
    delete_certs();
    testCertUpdate = CertificateUpdate();
    optionalTestCertUpdate = Optional<CertificateUpdate>(testCertUpdate);
    testResponse = new CertificateUpdateEvent();
    caCerts = Vector<String>(testCACerts.begin(), testCACerts.end());
}

void CertificateUpdatesHandlerTester::TearDown() {
    delete_certs();

    if (handler) {
        delete handler;
    }
    if (testResponse) {
        delete testResponse;
    }
}

TEST_F(CertificateUpdatesHandlerTester, OnStreamEventTestNullptr) {
    handler = new CertificateUpdatesHandler(std::move(testPath), nullptr);
    handler->OnStreamEvent(nullptr);
    EXPECT_FALSE(std::filesystem::exists(privateKeyFilePath));
    EXPECT_FALSE(std::filesystem::exists(certFilePath));
}

TEST_F(CertificateUpdatesHandlerTester, OnStreamEventTestNoUpdate) {
    handler = new CertificateUpdatesHandler(std::move(testPath), nullptr);
    handler->OnStreamEvent(testResponse);
    EXPECT_FALSE(std::filesystem::exists(privateKeyFilePath));
    EXPECT_FALSE(std::filesystem::exists(certFilePath));
}

TEST_F(CertificateUpdatesHandlerTester, OnStreamEventTestValidResponse) {
    optionalTestCertUpdate->SetPrivateKey(testPrivateKey.c_str());
    optionalTestCertUpdate->SetCertificate(testCert.c_str());
    optionalTestCertUpdate->SetCaCertificates(caCerts);
    testResponse->SetCertificateUpdate(optionalTestCertUpdate.value());

    auto subscription_callback = std::make_unique<std::function<void(CertificateUpdateEvent *)>>(
        [](CertificateUpdateEvent *) { std::cout << "callback" << std::endl; });
    handler = new CertificateUpdatesHandler(std::move(testPath), std::move(subscription_callback));
    handler->OnStreamEvent(testResponse);
    EXPECT_TRUE(std::filesystem::exists(privateKeyFilePath));
    EXPECT_EQ(readLines(privateKeyFilePath).front(), testPrivateKey);

    auto expectedCertChain = std::vector<std::string>(testCACerts);
    expectedCertChain.insert(expectedCertChain.begin(), testCert);

    EXPECT_TRUE(std::filesystem::exists(certFilePath));
    EXPECT_EQ(readLines(certFilePath), expectedCertChain);
}

TEST_F(CertificateUpdatesHandlerTester, OnStreamEventTestNoPrivateKey) {
    optionalTestCertUpdate->SetCertificate(testCert.c_str());
    optionalTestCertUpdate->SetCaCertificates(caCerts);
    testResponse->SetCertificateUpdate(optionalTestCertUpdate.value());

    handler = new CertificateUpdatesHandler(std::move(testPath), nullptr);
    handler->OnStreamEvent(testResponse);
    EXPECT_FALSE(std::filesystem::exists(privateKeyFilePath));
    EXPECT_FALSE(std::filesystem::exists(certFilePath));
}

TEST_F(CertificateUpdatesHandlerTester, OnStreamEventTestNoCert) {
    optionalTestCertUpdate->SetPrivateKey(testPrivateKey.c_str());
    optionalTestCertUpdate->SetCaCertificates(caCerts);
    testResponse->SetCertificateUpdate(optionalTestCertUpdate.value());

    handler = new CertificateUpdatesHandler(std::move(testPath), nullptr);
    handler->OnStreamEvent(testResponse);
    EXPECT_FALSE(std::filesystem::exists(privateKeyFilePath));
    EXPECT_FALSE(std::filesystem::exists(certFilePath));
}

TEST_F(CertificateUpdatesHandlerTester, OnStreamEventTestNoCaCert) {
    optionalTestCertUpdate->SetPrivateKey(testPrivateKey.c_str());
    optionalTestCertUpdate->SetCertificate(testCert.c_str());
    testResponse->SetCertificateUpdate(optionalTestCertUpdate.value());

    handler = new CertificateUpdatesHandler(std::move(testPath), nullptr);
    handler->OnStreamEvent(testResponse);
    EXPECT_FALSE(std::filesystem::exists(privateKeyFilePath));
    EXPECT_FALSE(std::filesystem::exists(certFilePath));
}

TEST_F(CertificateUpdatesHandlerTester, OnStreamEventTestInvalidCertWrite) {
    optionalTestCertUpdate->SetPrivateKey(testPrivateKey.c_str());
    optionalTestCertUpdate->SetCertificate(testCert.c_str());
    optionalTestCertUpdate->SetCaCertificates(caCerts);
    testResponse->SetCertificateUpdate(optionalTestCertUpdate.value());

    handler = new CertificateUpdatesHandler(nullptr, nullptr);
    handler->OnStreamEvent(testResponse);
    EXPECT_FALSE(std::filesystem::exists(privateKeyFilePath));
    EXPECT_FALSE(std::filesystem::exists(certFilePath));
}

TEST_F(CertificateUpdatesHandlerTester, WriteCertsToFilesTestNullBasePath) {
    handler = new CertificateUpdatesHandler(nullptr, nullptr);

    String crtPrivateKeyString(testPrivateKey);
    String crtCertString(testCert);

    CertWriteStatus retVal = handler->writeCertsToFiles(crtPrivateKeyString, crtCertString, caCerts);
    EXPECT_EQ(retVal, CertWriteStatus::WRITE_ERROR_BASE_PATH);
    EXPECT_FALSE(std::filesystem::exists(privateKeyFilePath));
    EXPECT_FALSE(std::filesystem::exists(certFilePath));
}

TEST_F(CertificateUpdatesHandlerTester, WriteCertsToFilesTestInvalidDir) {
    std::unique_ptr<std::filesystem::path> testPath = std::make_unique<std::filesystem::path>("");
    handler = new CertificateUpdatesHandler(std::move(testPath), nullptr);

    String crtPrivateKeyString(testPrivateKey);
    String crtCertString(testCert);

    CertWriteStatus retVal = handler->writeCertsToFiles(crtPrivateKeyString, crtCertString, caCerts);
    EXPECT_EQ(retVal, CertWriteStatus::WRITE_ERROR_DIR_PATH);
    EXPECT_FALSE(std::filesystem::exists(privateKeyFilePath));
    EXPECT_FALSE(std::filesystem::exists(certFilePath));
}

} // namespace unit
} // namespace tests
} // namespace port_driver
