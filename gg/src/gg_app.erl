%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(gg_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  {ok, Sup} = gg_sup:start_link(),
  gg_port_driver:start(),
  gg_conf:start(),
  gg_certs:request_certificates(),
  %% update config again to ensure "listeners" config is applied.
  %% "listeners" config update will fail the first time
  %% this component runs, because cert and key haven't been written yet,
  %% and emqx validates that they exist.
  case gg_conf:request_update_sync() of
    ok -> ok;
    Error -> exit(Error)
  end,
  gg:load(application:get_all_env()),
  {ok, Sup}.

stop(_State) ->
  gg_conf:stop(),
  gg:unload(),
  gg_port_driver:stop().
