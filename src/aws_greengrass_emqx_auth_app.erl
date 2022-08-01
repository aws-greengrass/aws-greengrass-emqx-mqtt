%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_auth_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-type(status() :: active | inactive).

-import(port_driver_integration, [start/0, stop/0]).
-import(tls_custom_certificate_verification, [enable/0, disable/0]).
-import(emqx_listeners, [find_by_id/1, stop_listener/1, start_listener/1, update_listeners_env/2]).

-export([start/2, stop/1]).
-export([plugin_status/0, activate/0, deactivate/0]).

%% Retrieve the current status of the greengrass auth plugin.
-spec(plugin_status() -> status()).
plugin_status() ->
  case application:get_env(aws_greengrass_emqx_auth, status) of
    undefined -> inactive;
    {ok, Val} -> Val
  end.

%% Set the status of of the greengrass auth plugin.
-spec(set_plugin_status(NewStatus :: status()) -> ok).
set_plugin_status(NewStatus) when is_atom(NewStatus) ->
  application:set_env(aws_greengrass_emqx_auth, status, NewStatus).

%% Retrieve the initial user-configured value for plugin status.
-spec(plugin_status_from_conf() -> status()).
plugin_status_from_conf() ->
  active. %% TODO implementation

start(_StartType, _StartArgs) ->
  {ok, Sup} = aws_greengrass_emqx_auth_sup:start_link(),
  port_driver_integration:start(),
  case plugin_status_from_conf() of
    active -> activate();
    inactive -> deactivate()
  end,
  aws_greengrass_emqx_auth:load(application:get_all_env()),
  {ok, Sup}.

stop(_State) ->
  deactivate(),
  aws_greengrass_emqx_auth:unload(),
  port_driver_integration:stop().

%% Enable the plugin to perform authN/authZ.
%% This operation is omnipotent.
-spec(activate() -> ok).
activate() -> %% TODO call when plugin_status changes to active
  case plugin_status() of
    active ->
      ok;
    inactive ->
      start_certificate_verification(),
      set_plugin_status(active),
      aws_greengrass_emqx_certs:load(),
      ok
  end.

%% Stop the plugin from performing authN/authZ, deferring to other applications in the
%% auth chain.
%%
%% NOTE: port_driver integration will stay enabled, to allow the plugin to react to
%%       config changes, allowing future re-activation.
%%
%% This operation is omnipotent.
-spec(deactivate() -> ok).
deactivate() -> %% TODO call when plugin_status changes to inactive
  case plugin_status() of
    active ->
      stop_certificate_verification(),
      set_plugin_status(inactive);
    inactive ->
      ok
  end.

-spec(start_certificate_verification() -> ok).
start_certificate_verification() ->
  case tls_custom_certificate_verification:enable() of
    ok ->
      ok;
    nossl ->
      throw({error, "Could not find active SSL listener"});
    {error, Reason} ->
      Err = io_lib:format("Failed to enable SSL custom certificate verification. Error: ~s", [Reason]),
      throw({error, Err})
  end.

-spec(stop_certificate_verification() -> ok).
stop_certificate_verification() ->
  case tls_custom_certificate_verification:disable() of
    ok ->
      ok;
    nossl ->
      ok;
    {error, Reason} ->
      Err = io_lib:format("Failed to disable SSL custom certificate verification. Error: ~s", [Reason]),
      throw({error, Err})
  end.
