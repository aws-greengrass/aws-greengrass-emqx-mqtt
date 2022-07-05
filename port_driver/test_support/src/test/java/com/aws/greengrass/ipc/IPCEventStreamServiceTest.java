/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package com.aws.greengrass.ipc;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import software.amazon.awssdk.aws.greengrass.GeneratedAbstractAuthorizeClientDeviceActionOperationHandler;
import software.amazon.awssdk.aws.greengrass.GeneratedAbstractGetClientDeviceAuthTokenOperationHandler;
import software.amazon.awssdk.aws.greengrass.GeneratedAbstractUpdateStateOperationHandler;
import software.amazon.awssdk.aws.greengrass.GeneratedAbstractVerifyClientDeviceIdentityOperationHandler;
import software.amazon.awssdk.aws.greengrass.GreengrassCoreIPCService;
import software.amazon.awssdk.aws.greengrass.model.AuthorizeClientDeviceActionRequest;
import software.amazon.awssdk.aws.greengrass.model.AuthorizeClientDeviceActionResponse;
import software.amazon.awssdk.aws.greengrass.model.GetClientDeviceAuthTokenRequest;
import software.amazon.awssdk.aws.greengrass.model.GetClientDeviceAuthTokenResponse;
import software.amazon.awssdk.aws.greengrass.model.InvalidClientDeviceAuthTokenError;
import software.amazon.awssdk.aws.greengrass.model.InvalidCredentialError;
import software.amazon.awssdk.aws.greengrass.model.UpdateStateRequest;
import software.amazon.awssdk.aws.greengrass.model.UpdateStateResponse;
import software.amazon.awssdk.aws.greengrass.model.VerifyClientDeviceIdentityRequest;
import software.amazon.awssdk.aws.greengrass.model.VerifyClientDeviceIdentityResponse;
import software.amazon.awssdk.eventstreamrpc.model.EventStreamJsonMessage;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

class IPCEventStreamServiceTest {
    private IPCEventStreamService ipcEventStreamService;
    private final GreengrassCoreIPCService ipcService = new GreengrassCoreIPCService();
    private static final String fifoPath = "/tmp/ggIpcWrite.sock";
    private BufferedReader br;
    private String value = "";

    @BeforeEach
    public void setup() throws IOException {
        ipcEventStreamService = new IPCEventStreamService(ipcService);
        ipcEventStreamService.startup();

        ipcService.setUpdateStateHandler((context) -> new GeneratedAbstractUpdateStateOperationHandler(context) {
            @Override
            protected void onStreamClosed() {
            }
            @Override
            public UpdateStateResponse handleRequest(UpdateStateRequest request) {
                print("update_state");
                return new UpdateStateResponse();
            }
            @Override
            public void handleStreamEvent(EventStreamJsonMessage streamRequestEvent) {
            }
        });

        ipcService.setGetClientDeviceAuthTokenHandler((context) -> new GeneratedAbstractGetClientDeviceAuthTokenOperationHandler(context) {
            @Override
            protected void onStreamClosed() {
            }

            @Override
            public GetClientDeviceAuthTokenResponse handleRequest(GetClientDeviceAuthTokenRequest request) {
                print("get_client_device_auth_token");
                switch (value) {
                    case "with_value":
                        return new GetClientDeviceAuthTokenResponse().withClientDeviceAuthToken("token");
                    case "with_error":
                        throw new InvalidCredentialError("Bad");
                    case "with_timeout":
                        try {
                            // Default timeout in CPP is 5 seconds
                            Thread.sleep(6_000);
                        } catch (InterruptedException ignored) {
                        }
                }
                return new GetClientDeviceAuthTokenResponse();
            }

            @Override
            public void handleStreamEvent(EventStreamJsonMessage streamRequestEvent) {
            }
        });

        ipcService.setAuthorizeClientDeviceActionHandler((context) -> new GeneratedAbstractAuthorizeClientDeviceActionOperationHandler(context) {
            @Override
            protected void onStreamClosed() {
            }

            @Override
            public AuthorizeClientDeviceActionResponse handleRequest(AuthorizeClientDeviceActionRequest request) {
                print("authorize_client_device_action");

                switch (value) {
                    case "with_false":
                        return new AuthorizeClientDeviceActionResponse().withIsAuthorized(false);
                    case "with_true":
                        return new AuthorizeClientDeviceActionResponse().withIsAuthorized(true);
                    case "with_error":
                        throw new InvalidCredentialError("Bad");
                    case "with_bad_token_error":
                        throw new InvalidClientDeviceAuthTokenError("Bad 2");
                    case "with_timeout":
                        try {
                            // Default timeout in CPP is 5 seconds
                            Thread.sleep(6_000);
                        } catch (InterruptedException ignored) {
                        }
                }
                return new AuthorizeClientDeviceActionResponse();
            }

            @Override
            public void handleStreamEvent(EventStreamJsonMessage streamRequestEvent) {
            }
        });

        ipcService.setVerifyClientDeviceIdentityHandler((context) -> new GeneratedAbstractVerifyClientDeviceIdentityOperationHandler(context) {
            @Override
            protected void onStreamClosed() {
            }

            @Override
            public VerifyClientDeviceIdentityResponse handleRequest(VerifyClientDeviceIdentityRequest request) {
                print("verify_client_device_identity");

                switch (value) {
                    case "with_false":
                        return new VerifyClientDeviceIdentityResponse().withIsValidClientDevice(false);
                    case "with_true":
                        return new VerifyClientDeviceIdentityResponse().withIsValidClientDevice(true);
                    case "with_error":
                        throw new InvalidCredentialError("Bad");
                    case "with_timeout":
                        try {
                            // Default timeout in CPP is 5 seconds
                            Thread.sleep(6_000);
                        } catch (InterruptedException ignored) {
                        }
                }
                return new VerifyClientDeviceIdentityResponse();
            }

            @Override
            public void handleStreamEvent(EventStreamJsonMessage streamRequestEvent) {
            }
        });

        print("ipc_start");
        br = Files.newBufferedReader(Paths.get(fifoPath));
        print("pipe_open");
    }

    @AfterEach
    public void tearDown() {
        ipcEventStreamService.close();
        print("down");
    }

    void print(String f) {
        System.out.print("EMQX: ");
        System.out.println(f);
        System.out.flush();
    }

    void waitFor() throws IOException {
        String line = read();
        while (line != null) {
            if (line.startsWith("set ")) {
                value = line.substring(4);
                print("did_set");
            }
            line = read();
        }
    }

    String read() throws IOException {
        return br.readLine();
    }

    @Test
    void t1() throws IOException, InterruptedException {
        print("up");
        waitFor();
    }
}
