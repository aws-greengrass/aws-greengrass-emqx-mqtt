%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_auth_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-import(port_driver_integration, [start/0, stop/0]).
-import(tls_custom_certificate_verification, [enable/0, disable/0]).
-import(emqx_listeners, [find_by_id/1, stop_listener/1, start_listener/1, update_listeners_env/2]).
-import(aws_greengrass_emqx_conf, [auth_enabled/0, when_auth_enabled_changes/1]).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
  {ok, Sup} = aws_greengrass_emqx_auth_sup:start_link(),
  port_driver_integration:start(),
  auth_enabled() andalso enable_certificate_verification(),
  when_auth_enabled_changes(fun (AuthEnabled) ->
    case AuthEnabled of
      true -> enable_certificate_verification();
      false -> disable_certificate_verification()
    end
  end),
  aws_greengrass_emqx_auth:load(application:get_all_env()),
  {ok, Sup}.

stop(_State) ->
  aws_greengrass_emqx_auth:unload(),
  port_driver_integration:stop().


-spec(enable_certificate_verification() -> ok).
enable_certificate_verification() ->
  case tls_custom_certificate_verification:enable() of
    ok ->
      aws_greengrass_emqx_certs:load(),
      ok;
    nossl ->
      throw({error, "Could not find active SSL listener"});
    {error, Reason} ->
      Err = io_lib:format("Failed to enable SSL custom certificate verification. Error: ~s", [Reason]),
      throw({error, Err})
  end.

-spec(disable_certificate_verification() -> ok).
disable_certificate_verification() ->
  case tls_custom_certificate_verification:disable() of
    ok ->
      ok;
    nossl ->
      ok;
    {error, Reason} ->
      Err = io_lib:format("Failed to disable SSL custom certificate verification. Error: ~s", [Reason]),
      throw({error, Err})
  end.
