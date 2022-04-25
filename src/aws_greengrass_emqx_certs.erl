%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_certs).

-include("emqx.hrl").

-export([ load/1
]).


%% Called when the plugin application start
load(Env) ->
  io:fwrite("TEsting avipinku\n")ddf
%%  emqx:hook('client.connect',      {?MODULE, on_client_connect, [Env]}),
%%  emqx:hook('client.connected',    {?MODULE, on_client_connected, [Env]}),
%%  emqx:hook('client.disconnected', {?MODULE, on_client_disconnected, [Env]}),
%%  emqx:hook('client.authenticate', {?MODULE, on_client_authenticate, [Env]}),
%%  emqx:hook('client.check_acl',    {?MODULE, on_client_check_acl, [Env]}).
