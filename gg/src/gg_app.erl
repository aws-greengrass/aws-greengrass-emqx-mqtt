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
  enable_cert_verification(),
  gg_certs:load(),
  gg:load(application:get_all_env()),
  {ok, Sup}.

enable_cert_verification() ->
  case gg_conf:auth_mode() of
    bypass ->
      logger:info("Skipping custom cert verification");
    _ ->
      case gg_tls:enable(mtls) of
        ok ->
          logger:info("Custom cert verification enabled");
        {error, Reason} ->
          ErrorString = io_lib:format("Failed to enable SSL custom certificate verification. Error: ~s", [Reason]),
          throw({error, ErrorString})
      end
  end.

stop(_State) ->
  gg_conf:stop(),
  gg:unload(),
  gg_port_driver:stop().
