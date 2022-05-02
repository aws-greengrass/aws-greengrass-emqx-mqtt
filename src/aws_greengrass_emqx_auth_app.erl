%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_auth_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-import(port_driver_integration,[start/0, stop/0]).

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
%%  Uncomment to load server certs in from the filesystem
  aws_greengrass_emqx_certs:load(),
  {ok, Sup} = aws_greengrass_emqx_auth_sup:start_link(),
  port_driver_integration:start(),
  aws_greengrass_emqx_auth:load(application:get_all_env()),
  {ok, Sup}.

stop(_State) ->
    aws_greengrass_emqx_auth:unload(),
    port_driver_integration:stop().
