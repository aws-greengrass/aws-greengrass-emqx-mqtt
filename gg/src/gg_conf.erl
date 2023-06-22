%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(gg_conf).

-export([auth_mode/0, use_greengrass_managed_certificates/0]).

-export([start/0, stop/0]).
-export([listen_for_update_requests/1, do_listen_for_update_requests/1, request_update/0]).

-type(auth_mode() :: enabled | bypass_on_failure | bypass).
-type(use_greengrass_managed_certificates() :: true | false).

-export_type([auth_mode/0, use_greengrass_managed_certificates/0]).

-define(INITIAL_CONF_TIMEOUT_MILLIS, 30000).

-define(OVERRIDE_TYPE, local).
-define(CONF_OPTS, #{override_to => ?OVERRIDE_TYPE}).
-define(ENV_APP, aws_greengrass_emqx_auth).
-define(UPDATE_PROC, gg_conf_listen_for_updates).
-define(CONF_UPDATE, conf_updated).

%% config keys
-define(KEY_ROOT, <<"aws_greengrass_emqx_auth">>).
-define(KEY_AUTH_MODE, <<"auth_mode">>).
-define(KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, <<"use_greengrass_managed_certificates">>).
-define(NORMALIZE_MAP, fun emqx_utils_maps:binary_key_map/1). %% configs like authentication only work with binary keys
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
    ?CONF_UPDATE -> ok
  after ?INITIAL_CONF_TIMEOUT_MILLIS ->
    exit({error, "Timed out waiting for initial configuration"})
  end.

stop() ->
  ?UPDATE_PROC ! stop.

%% Retrieve Greengrass configuration via GetConfiguration IPC call,
%% and update emqx override configuration with the result.
request_update() ->
  ?UPDATE_PROC ! update.

listen_for_update_requests(NotifyPID) ->
  ListenPID = spawn(?MODULE, do_listen_for_update_requests, [NotifyPID]),
  register(?UPDATE_PROC, ListenPID),
  gg_port_driver:subscribe_to_configuration_updates(fun request_update/0),
  request_update().

do_listen_for_update_requests(NotifyPID) ->
  receive
    update ->
      logger:info("Config update request received"),
      update_conf_from_ipc(NotifyPID),
      do_listen_for_update_requests(NotifyPID);
    stop -> ok
  end.

%%--------------------------------------------------------------------
%% Update Config
%%--------------------------------------------------------------------

update_conf_from_ipc(NotifyPID) ->
  case gg_port_driver:get_configuration() of
  {ok, Conf} ->
    update_conf(Conf),
    NotifyPID ! ?CONF_UPDATE,
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

update_conf(Existing, New) ->
  ExistingConf = ?NORMALIZE_MAP(Existing),
  NewConf = ?NORMALIZE_MAP(New),

  %% update greengrass plugin configuration
  ExistingPluginConf = map_with_root_key(ExistingConf, ?KEY_ROOT),
  NewPluginConf = map_with_root_key(NewConf, ?KEY_ROOT),
  logger:debug("Updating plugin config: ExistingConf=~p, NewConf=~p", [ExistingPluginConf, NewPluginConf]),
  try update_plugin_conf(ExistingPluginConf, NewPluginConf)
  catch
    PluginUpdateErr -> logger:warning("Unable to update plugin configuration: ~p", [PluginUpdateErr])
  end,

  %% update emqx override configuration
  ExistingOverrideConf = map_without_root_key(ExistingConf, ?KEY_ROOT),
  NewOverrideConf = map_without_root_key(NewConf, ?KEY_ROOT),
  logger:debug("Updating override config: ExistingConf=~p, NewConf=~p", [ExistingOverrideConf, NewOverrideConf]),
  try update_override_conf(ExistingOverrideConf, NewOverrideConf)
  catch
    OverrideUpdateErr -> logger:warning("Unable to update emqx override configuration: ~p", [OverrideUpdateErr])
  end.

%%--------------------------------------------------------------------
%% Update Plugin Config
%%--------------------------------------------------------------------

%% TODO trigger actions on config change
update_plugin_conf(_, #{} = NewConf) when map_size(NewConf) == 0 ->
  clear_plugin_conf();
update_plugin_conf(_, NewConf) ->
  CheckedConf = validate_plugin_conf(NewConf),
  update_plugin_env(CheckedConf).

clear_plugin_conf() ->
  application:set_env(?ENV_APP, ?KEY_AUTH_MODE, ?DEFAULT_AUTH_MODE),
  application:set_env(?ENV_APP, ?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, ?DEFAULT_USE_GREENGRASS_MANAGED_CERTIFICATES).

validate_plugin_conf(Conf) ->
  try
    {_, CheckedConf} = hocon_tconf:map_translate(gg_schema, Conf, #{return_plain => true, format => map}),
    ?NORMALIZE_MAP(CheckedConf)
  catch throw:E:ST ->
    {error, {config_validation, E, ST}}
  end.

update_plugin_env(Conf) ->
  %% TODO find a more dynamic way
  RootConfig = maps:get(?KEY_ROOT, Conf),
  AuthMode = maps:get(?KEY_AUTH_MODE, RootConfig, ?DEFAULT_AUTH_MODE),
  application:set_env(?ENV_APP, ?KEY_AUTH_MODE, AuthMode),
  logger:info("Updated ~p plugin config to ~p", [[?ENV_APP, ?KEY_AUTH_MODE], AuthMode]),
  UseGreengrassManagedCertificates = maps:get(?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, RootConfig, ?DEFAULT_USE_GREENGRASS_MANAGED_CERTIFICATES),
  application:set_env(?ENV_APP, ?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, UseGreengrassManagedCertificates),
  logger:info("Updated ~p plugin config to ~p", [[?ENV_APP, ?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES], UseGreengrassManagedCertificates]).

%%--------------------------------------------------------------------
%% Update EMQX Override Config
%%--------------------------------------------------------------------

update_override_conf(ExistingConf, #{} = NewConf) when map_size(NewConf) == 0 ->
  clear_override_conf(ExistingConf);
update_override_conf(ExistingConf, NewConf) ->
  MapToClear = maps:filter(fun(K,_) -> not maps:is_key(K, NewConf) end, ExistingConf),
  clear_override_conf(MapToClear),
  update_override_conf(ExistingConf, NewConf, maps:keys(NewConf)).

update_override_conf(_, _, []) ->
  ok;
update_override_conf(ExistingConf, NewConf, [Key | Rest]) ->
  ConfPath = [Key],
  case catch emqx_conf:update(ConfPath, maps:get(Key, NewConf), ?CONF_OPTS) of
    {ok, _} -> logger:info("Updated ~p config", [ConfPath]);
    {error, UpdateError} -> logger:warning("Failed to update configuration. confPath=~p, error=~p", [ConfPath, UpdateError]);
    Err -> logger:warning("Failed to update configuration. confPath=~p, error=~p", [ConfPath, Err])
  end,
  update_override_conf(ExistingConf, NewConf, Rest).

clear_override_conf(ExistingConf) when is_map(ExistingConf) ->
  %% we must remove leaf configs because EMQX does not allow use to remove root configs.
  for_each_conf(ExistingConf, fun remove_override_conf_path/1).

remove_override_conf_path([]) ->
  ok;
remove_override_conf_path(Path) ->
  remove_override_conf(Path),
  remove_override_conf_path(lists:droplast(Path)).

remove_override_conf([]) ->
  ok;
remove_override_conf(Path) when is_list(Path) ->
  case catch emqx_conf:remove(Path, ?CONF_OPTS) of
    {ok, _} -> logger:info("Removed ~p config", [Path]);
    {error, RemoveError} -> logger:warning("Failed to remove configuration. confPath=~p, error=~p", [Path, RemoveError]);
    Err -> logger:warning("Failed to remove configuration. confPath=~p, error=~p", [Path, Err])
  end.

get_override_conf() ->
  emqx_config:read_override_conf(?CONF_OPTS).

for_each_conf(Conf, Func) ->
  for_each_conf([], Conf, Func).
for_each_conf(Path, Conf, Func) ->
  if
    map_size(Conf) == 0 -> Func(Path);
    true -> maps:foreach(
      fun(K, V) ->
        if
          is_map(V) -> for_each_conf(Path ++ [K], V, Func);
          true -> Func(Path ++ [K])
        end
      end, Conf)
  end.

map_with_root_key(Conf, Key) ->
  maps:filter(fun(K, _) -> K == Key end, Conf).

map_without_root_key(Conf, Key) ->
  maps:filter(fun(K, _) -> K =/= Key end, Conf).
