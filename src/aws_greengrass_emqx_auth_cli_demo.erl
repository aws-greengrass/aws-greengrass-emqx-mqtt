%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

%% TODO: Remove CLI code

-module(aws_greengrass_emqx_auth_cli_demo).

-export([cmd/1]).

cmd(["arg1", "arg2"]) ->
  emqx_ctl:print("ok");

cmd(_) ->
  emqx_ctl:usage([{"cmd arg1 arg2", "cmd demo"}]).
