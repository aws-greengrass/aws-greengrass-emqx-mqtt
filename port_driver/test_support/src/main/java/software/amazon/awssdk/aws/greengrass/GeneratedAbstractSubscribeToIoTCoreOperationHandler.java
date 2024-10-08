/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package software.amazon.awssdk.aws.greengrass;

import software.amazon.awssdk.aws.greengrass.model.IoTCoreMessage;
import software.amazon.awssdk.aws.greengrass.model.SubscribeToIoTCoreRequest;
import software.amazon.awssdk.aws.greengrass.model.SubscribeToIoTCoreResponse;
import software.amazon.awssdk.eventstreamrpc.OperationContinuationHandler;
import software.amazon.awssdk.eventstreamrpc.OperationContinuationHandlerContext;
import software.amazon.awssdk.eventstreamrpc.OperationModelContext;
import software.amazon.awssdk.eventstreamrpc.model.EventStreamJsonMessage;

public abstract class GeneratedAbstractSubscribeToIoTCoreOperationHandler extends OperationContinuationHandler<SubscribeToIoTCoreRequest, SubscribeToIoTCoreResponse, EventStreamJsonMessage, IoTCoreMessage> {
  protected GeneratedAbstractSubscribeToIoTCoreOperationHandler(
      OperationContinuationHandlerContext context) {
    super(context);
  }

  @Override
  public OperationModelContext<SubscribeToIoTCoreRequest, SubscribeToIoTCoreResponse, EventStreamJsonMessage, IoTCoreMessage> getOperationModelContext(
      ) {
    return GreengrassCoreIPCServiceModel.getSubscribeToIoTCoreModelContext();
  }
}
