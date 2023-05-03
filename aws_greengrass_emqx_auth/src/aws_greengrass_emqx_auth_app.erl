%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_auth_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  {ok, Sup} = aws_greengrass_emqx_auth_sup:start_link(),
  port_driver_integration:start(),
  enable_cert_verification(),
  aws_greengrass_emqx_certs:load(),
  aws_greengrass_emqx_auth:load(application:get_all_env()),
  {ok, Sup}.

enable_cert_verification() ->
  case aws_greengrass_emqx_conf:auth_mode() of
    bypass ->
      logger:info("Skipping custom cert verification");
    _ ->
      case tls_custom_certificate_verification:enable("mlts") of
        ok ->
          logger:info("Custom cert verification enabled");
        {error, Reason} ->
          ErrorString = io_lib:format("Failed to enable SSL custom certificate verification. Error: ~s", [Reason]),
          throw({error, ErrorString})
      end
  end.

stop(_State) ->
  aws_greengrass_emqx_auth:unload(),
  port_driver_integration:stop().
