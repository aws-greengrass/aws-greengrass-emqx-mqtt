/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package com.aws.greengrass.ipc;

import com.aws.greengrass.ipc.common.DefaultOperationHandler;
import software.amazon.awssdk.aws.greengrass.GreengrassCoreIPCService;
import software.amazon.awssdk.aws.greengrass.GreengrassCoreIPCServiceModel;
import software.amazon.awssdk.crt.eventstream.Header;
import software.amazon.awssdk.crt.io.EventLoopGroup;
import software.amazon.awssdk.crt.io.SocketOptions;
import software.amazon.awssdk.eventstreamrpc.AuthenticationData;
import software.amazon.awssdk.eventstreamrpc.Authorization;
import software.amazon.awssdk.eventstreamrpc.RpcServer;

import java.io.Closeable;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFilePermission;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

public class IPCEventStreamService implements Closeable {
    public static final int DEFAULT_PORT_NUMBER = 8033;

    private RpcServer rpcServer;

    private final GreengrassCoreIPCService greengrassCoreIPCService;

    private SocketOptions socketOptions;
    private EventLoopGroup eventLoopGroup;

    IPCEventStreamService(GreengrassCoreIPCService greengrassCoreIPCService) {
        this.greengrassCoreIPCService = greengrassCoreIPCService;
    }

    public void startup() {
        try {
            greengrassCoreIPCService.getAllOperations().forEach(operation ->
                    greengrassCoreIPCService.setOperationHandler(operation,
                    (context) -> new DefaultOperationHandler(GreengrassCoreIPCServiceModel.getInstance()
                            .getOperationModelContext(operation), context)));
            greengrassCoreIPCService.setAuthenticationHandler((List<Header> headers, byte[] bytes) ->
                    ipcAuthenticationHandler(bytes));
            greengrassCoreIPCService.setAuthorizationHandler(this::ipcAuthorizationHandler);

            socketOptions = new SocketOptions();
            socketOptions.connectTimeoutMs = 3000;
            socketOptions.domain = SocketOptions.SocketDomain.LOCAL;
            socketOptions.type = SocketOptions.SocketType.STREAM;
            eventLoopGroup = new EventLoopGroup(1);

            Files.deleteIfExists(Paths.get("/tmp/ggIpc.sock"));
            rpcServer = new RpcServer(eventLoopGroup, socketOptions, null, "/tmp/ggIpc.sock",
                    DEFAULT_PORT_NUMBER, greengrassCoreIPCService);
            rpcServer.runServer();
        } catch (RuntimeException | IOException e) {
            // Make sure to cleanup anything we created since we don't know where exactly we failed
            close();
            throw new RuntimeException(e);
        }

        // Wait for the socket to be created
        Path ipcPath = Paths.get("/tmp/ggIpc.sock");
        long maxTime = System.currentTimeMillis() + (30 * 1000);
        while (System.currentTimeMillis() < maxTime && Files.notExists(ipcPath)) {
            try {
                Thread.sleep(500);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
        try {
            Files.setPosixFilePermissions(ipcPath,
                    new HashSet<>(Arrays.asList(PosixFilePermission.OTHERS_READ, PosixFilePermission.OTHERS_EXECUTE,
                            PosixFilePermission.OTHERS_WRITE, PosixFilePermission.OWNER_EXECUTE,
                            PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE,
                            PosixFilePermission.GROUP_READ, PosixFilePermission.GROUP_WRITE,
                            PosixFilePermission.GROUP_EXECUTE)));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private Authorization ipcAuthorizationHandler(AuthenticationData authenticationData) {
        // No authorization on service level exist for whole IPC right now so returning ACCEPT for all authenticated
        // clients
        return Authorization.ACCEPT;
    }

    private AuthenticationData ipcAuthenticationHandler(byte[] payload) {
        return () -> "";
    }

    @Override
    public void close() {
        if (rpcServer != null) {
            rpcServer.stopServer();
        }
        if (eventLoopGroup != null) {
            eventLoopGroup.close();
        }
        if (socketOptions != null) {
            socketOptions.close();
        }
    }
}
