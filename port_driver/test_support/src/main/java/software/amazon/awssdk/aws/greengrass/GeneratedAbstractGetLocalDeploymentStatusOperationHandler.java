/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package software.amazon.awssdk.aws.greengrass;

import software.amazon.awssdk.aws.greengrass.model.GetLocalDeploymentStatusRequest;
import software.amazon.awssdk.aws.greengrass.model.GetLocalDeploymentStatusResponse;
import software.amazon.awssdk.eventstreamrpc.OperationContinuationHandler;
import software.amazon.awssdk.eventstreamrpc.OperationContinuationHandlerContext;
import software.amazon.awssdk.eventstreamrpc.OperationModelContext;
import software.amazon.awssdk.eventstreamrpc.model.EventStreamJsonMessage;

public abstract class GeneratedAbstractGetLocalDeploymentStatusOperationHandler extends OperationContinuationHandler<GetLocalDeploymentStatusRequest, GetLocalDeploymentStatusResponse, EventStreamJsonMessage, EventStreamJsonMessage> {
  protected GeneratedAbstractGetLocalDeploymentStatusOperationHandler(
      OperationContinuationHandlerContext context) {
    super(context);
  }

  @Override
  public OperationModelContext<GetLocalDeploymentStatusRequest, GetLocalDeploymentStatusResponse, EventStreamJsonMessage, EventStreamJsonMessage> getOperationModelContext(
      ) {
    return GreengrassCoreIPCServiceModel.getGetLocalDeploymentStatusModelContext();
  }
}
