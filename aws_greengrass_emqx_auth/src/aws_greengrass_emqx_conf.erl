%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_conf).

-export([auth_mode/0, use_greengrass_managed_certificates/0]).

-export([start/0, stop/0]).
-export([listen_for_update_requests/1, request_update/0]).

-export([update_configuration_from_ipc/0]).

-type(auth_mode() :: enabled | bypass_on_failure | bypass).
-type(use_greengrass_managed_certificates() :: true | false).

-export_type([auth_mode/0, use_greengrass_managed_certificates/0]).

-define(ENV_APP, aws_greengrass_emqx_auth).
-define(UPDATE_PROC, greengrass_config_update_listener).

%% config keys
-define(KEY_AUTH_MODE, auth_mode).
-define(KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, use_greengrass_managed_certificates).

%% default config values
-define(DEFAULT_AUTH_MODE, enabled).
-define(DEFAULT_USE_GREENGRASS_MANAGED_CERTIFICATES, true).


%%--------------------------------------------------------------------
%% Config API
%%--------------------------------------------------------------------

-spec(auth_mode() -> auth_mode()).
auth_mode() ->
  application:get_env(?ENV_APP, ?KEY_AUTH_MODE, ?DEFAULT_AUTH_MODE).

-spec(use_greengrass_managed_certificates() -> use_greengrass_managed_certificates()).
use_greengrass_managed_certificates() ->
  application:get_env(?ENV_APP, ?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, ?DEFAULT_USE_GREENGRASS_MANAGED_CERTIFICATES).

%%--------------------------------------------------------------------
%% Config Loading
%%--------------------------------------------------------------------

start() ->
  spawn(?MODULE, listen_for_update_requests, [self()]),
  receive
    initialized -> ok
  end.

stop() ->
  ?UPDATE_PROC ! stop.

%% Retrieve Greengrass configuration via GetConfiguration IPC call,
%% and update emqx override configuration with the result.
request_update() ->
  ?UPDATE_PROC ! update.

listen_for_update_requests(Caller) ->
  register(?UPDATE_PROC, self()),
  Caller ! initialized,
  do_listen_for_update_requests().

do_listen_for_update_requests() ->
  receive
    update ->
      logger:info("Config update request received"),
      update_configuration_from_ipc(),
      do_listen_for_update_requests();
    stop -> ok
  end.

update_configuration_from_ipc() ->
  case port_driver_integration:get_configuration() of
  {ok, Conf} ->
    update_configuration(Conf),
    ok;
  {error, Err} = Error ->
    logger:error("Unable to get configuration, err=~p", [Err]),
    Error
  end.

update_configuration(Conf) when is_binary(Conf) ->
  case catch jiffy:decode(Conf, [return_maps, dedupe_keys, use_nil]) of
    DecodedConf when is_map(DecodedConf) -> update_configuration(DecodedConf);
    Err -> logger:warning("Unable to decode configuration: error=~p", [Err])
  end;
update_configuration(Conf) ->
  update_configuration(Conf, maps:keys(Conf)).

update_configuration(_, []) ->
  ok;
update_configuration(Conf, [Key | Rest]) when Key == "aws_greengrass_emqx_auth"; Key == <<"aws_greengrass_emqx_auth">> ->
  PluginConf = maps:filter(fun(K, _) -> K == Key end, Conf),
  update_plugin_config(PluginConf),
  update_configuration(Conf, Rest);
update_configuration(Conf, [Key | Rest]) ->
  ConfPath = [Key],
  case catch emqx_conf:update(ConfPath, maps:get(Key, Conf), #{rawconf_with_defaults => true, override_to => local}) of
    {ok, _} -> logger:info("Updated ~p config", [ConfPath]);
    {error, UpdateError} -> logger:warning("Failed to update configuration. confPath=~p, error=~p", [ConfPath, UpdateError]);
    Err -> logger:warning("Failed to update configuration. confPath=~p, error=~p", [ConfPath, Err])
  end,
  update_configuration(Conf, Rest).

update_plugin_config(Conf) ->
  CheckedConf = validate_plugin_conf(Conf),
  update_plugin_env(CheckedConf).

validate_plugin_conf(Conf) ->
  try
    {_, CheckedConf} = hocon_tconf:map_translate(aws_greengrass_emqx_auth_schema, Conf, #{return_plain => true, format => map}),
    emqx_map_lib:unsafe_atom_key_map(CheckedConf)
  catch throw:E:ST ->
    {error, {config_validation, E, ST}}
  end.

update_plugin_env(Conf) ->
  %% TODO find a more dynamic way
  RootConfig = maps:get(aws_greengrass_emqx_auth, Conf),
  AuthMode = maps:get(?KEY_AUTH_MODE, RootConfig, ?DEFAULT_AUTH_MODE),
  application:set_env(?ENV_APP, ?KEY_AUTH_MODE, AuthMode),
  logger:info("Updated ~p plugin config to ~p", [[?ENV_APP, ?KEY_AUTH_MODE], AuthMode]),
  UseGreengrassManagedCertificates = maps:get(?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, RootConfig, ?DEFAULT_USE_GREENGRASS_MANAGED_CERTIFICATES),
  application:set_env(?ENV_APP, ?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, UseGreengrassManagedCertificates),
  logger:info("Updated ~p plugin config to ~p", [[?ENV_APP, ?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES], UseGreengrassManagedCertificates]).
