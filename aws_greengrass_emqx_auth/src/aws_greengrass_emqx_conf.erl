%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_conf).

-export([auth_mode/0, use_greengrass_managed_certificates/0]).

-export([start/0, stop/0]).
-export([listen_for_update_requests/1, request_update/0]).

-type(auth_mode() :: enabled | bypass_on_failure | bypass).
-type(use_greengrass_managed_certificates() :: true | false).

-export_type([auth_mode/0, use_greengrass_managed_certificates/0]).

-define(OVERRIDE_TYPE, local).
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
%% Config Update Listener
%%--------------------------------------------------------------------

%% Subscribe to configuration updates and perform a one time configuration update.
start() ->
  spawn(?MODULE, listen_for_update_requests, [self()]),
  receive
    initialized ->
      request_update(),
      %% TODO wait for completion!
      ok
  end.

stop() ->
  ?UPDATE_PROC ! stop.

%% Retrieve Greengrass configuration via GetConfiguration IPC call,
%% and update emqx override configuration with the result.
request_update() ->
  ?UPDATE_PROC ! update.

listen_for_update_requests(Caller) ->
  register(?UPDATE_PROC, self()),
  port_driver_integration:subscribe_to_configuration_updates(fun request_update/0),
  Caller ! initialized,
  do_listen_for_update_requests().

do_listen_for_update_requests() ->
  receive
    update ->
      logger:info("Config update request received"),
      update_conf_from_ipc(),
      do_listen_for_update_requests();
    stop -> ok
  end.

%%--------------------------------------------------------------------
%% Update Config
%%--------------------------------------------------------------------

update_conf_from_ipc() ->
  case port_driver_integration:get_configuration() of
  {ok, Conf} ->
    update_conf(Conf),
    ok;
  {error, Err} = Error ->
    logger:error("Unable to get configuration, err=~p", [Err]),
    Error
  end.

update_conf(NewConf) when is_binary(NewConf) ->
  case decode_conf(NewConf) of
    {error, Err} -> logger:warning("Unable to decode configuration, skipping config update:  ~p", [Err]);
    DecodedConf -> update_conf(DecodedConf)
  end;
update_conf(NewConf) ->
  ExistingConf =
    case get_override_conf() of
      undefined -> #{};
      BadConf when is_list(BadConf) ->
        logger:warning("Unexpected list format found when retrieving existing configuration, treating as empty"),
        #{};
      Conf -> Conf
    end,
  update_conf(ExistingConf, NewConf).

decode_conf(Conf) when is_binary(Conf) ->
  case catch jiffy:decode(Conf, [return_maps, dedupe_keys, use_nil]) of
    DecodedConf when is_map(DecodedConf) -> DecodedConf;
    Err -> {error, Err}
  end.

update_conf(ExistingConf, NewConf) ->
  BinaryExistingConf = emqx_map_lib:binary_key_map(ExistingConf),
  BinaryNewConf = emqx_map_lib:binary_key_map(NewConf),

  %% update greengrass plugin configuration
  ExistingPluginConf = maps:get(<<"aws_greengrass_emqx_auth">>, BinaryExistingConf, #{}),
  NewPluginConf = maps:get(<<"aws_greengrass_emqx_auth">>, BinaryNewConf, #{}),
  try update_plugin_conf(ExistingPluginConf, NewPluginConf)
  catch
    Err -> logger:warning("Unable to update plugin configuration: ~p", [Err])
  end,

  %% update emqx override configuration
  ExistingOverrideConf = get_override_conf(),
  NewOverrideConf = maps:filter(fun(K, _) -> K =/= <<"aws_greengrass_emqx_auth">> end, BinaryNewConf),
  try update_override_conf(ExistingOverrideConf, NewOverrideConf)
  catch
    Err -> logger:warning("Unable to update emqx override configuration: ~p", [Err])
  end.

%%--------------------------------------------------------------------
%% Update Plugin Config
%%--------------------------------------------------------------------

%% TODO trigger actions on config change
update_plugin_conf(_, #{}) ->
  clear_plugin_conf();
update_plugin_conf(ExistingConf, NewConf) ->
  CheckedConf = validate_plugin_conf(NewConf),
  update_plugin_env(CheckedConf).

clear_plugin_conf() ->
  application:set_env(?ENV_APP, ?KEY_AUTH_MODE, ?DEFAULT_AUTH_MODE),
  application:set_env(?ENV_APP, ?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, ?DEFAULT_USE_GREENGRASS_MANAGED_CERTIFICATES).

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

%%--------------------------------------------------------------------
%% Update EMQX Override Config
%%--------------------------------------------------------------------

update_override_conf(ExistingConf, #{}) ->
  clear_override_conf(ExistingConf);
update_override_conf(ExistingConf, NewConf) ->
  KeysToClear = lists:filter(fun(Key) -> not maps:is_key(Key, maps:keys(NewConf)) end, maps:keys(ExistingConf)),
  clear_override_conf(KeysToClear),
  update_override_conf(ExistingConf, NewConf, maps:keys(NewConf)).

update_override_conf(_, _, []) ->
  ok;
update_override_conf(ExistingConf, NewConf, [Key | Rest]) ->
  ConfPath = [Key],
  case catch emqx_conf:update(ConfPath, maps:get(Key, NewConf), #{rawconf_with_defaults => true, override_to => ?OVERRIDE_TYPE}) of
    {ok, _} -> logger:info("Updated ~p config", [ConfPath]);
    {error, UpdateError} -> logger:warning("Failed to update configuration. confPath=~p, error=~p", [ConfPath, UpdateError]);
    Err -> logger:warning("Failed to update configuration. confPath=~p, error=~p", [ConfPath, Err])
  end,
  update_override_conf(ExistingConf, NewConf, Rest).

clear_override_conf(ExistingConf) when is_map(ExistingConf) ->
  clear_override_conf(maps:keys(ExistingConf));
clear_override_conf([]) ->
  ok;
clear_override_conf([Key | Rest]) ->
  emqx_conf:remove(Key, #{rawconf_with_defaults => true, override_to => ?OVERRIDE_TYPE}),
  clear_override_conf(Rest).

get_override_conf() ->
  emqx_config:read_override_conf(#{override_to => ?OVERRIDE_TYPE}).
