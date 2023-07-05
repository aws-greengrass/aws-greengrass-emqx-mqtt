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
-type(config_update_mode() :: merge | override).

-export_type([auth_mode/0, use_greengrass_managed_certificates/0]).

-define(INITIAL_CONF_TIMEOUT_MILLIS, 30000).

%% config keys
-define(KEY_CONFIG_UPDATE_MODE, <<"configUpdateMode">>).
-define(KEY_EMQX_CONFIG, <<"emqxConfig">>).
-define(KEY_AUTH_MODE, <<"authMode">>).
-define(KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, <<"useGreengrassManagedCertificates">>).
%% defaults
-define(DEFAULT_CONFIG_UPDATE_MODE, merge).
-define(DEFAULT_AUTH_MODE, enabled).
-define(DEFAULT_USE_GREENGRASS_MANAGED_CERTIFICATES, true).

-define(SCHEMA_ROOT, <<"gg">>).

-define(ENV_APP, aws_greengrass_emqx_auth).

-define(CONF_OPTS, #{override_to => cluster}).

-define(UPDATE_PROC, gg_conf_listen_for_updates).
-define(CONF_UPDATE, conf_updated).

-define(NORMALIZE_MAP, fun emqx_utils_maps:binary_key_map/1). %% configs like authentication only work with binary keys


%%--------------------------------------------------------------------
%% Config API
%%--------------------------------------------------------------------

-spec(auth_mode() -> auth_mode()).
auth_mode() ->
  application:get_env(?ENV_APP, ?KEY_AUTH_MODE, ?DEFAULT_AUTH_MODE).

-spec(use_greengrass_managed_certificates() -> use_greengrass_managed_certificates()).
use_greengrass_managed_certificates() ->
  application:get_env(?ENV_APP, ?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, ?DEFAULT_USE_GREENGRASS_MANAGED_CERTIFICATES).

-spec(config_update_mode() -> config_update_mode()).
config_update_mode() ->
  application:get_env(?ENV_APP, ?KEY_CONFIG_UPDATE_MODE, ?DEFAULT_CONFIG_UPDATE_MODE).


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

update_conf(NewComponentConf) when is_binary(NewComponentConf) ->
  case decode_conf(NewComponentConf) of
    {error, Err} -> logger:warning("Unable to decode configuration, skipping config update:  ~p", [Err]);
    DecodedConf -> update_conf(DecodedConf)
  end;
update_conf(NewComponentConf) ->
  ExistingOverrideConf =
    case get_override_conf() of
      undefined -> #{};
      BadConf when is_list(BadConf) ->
        logger:warning("Unexpected list format found when retrieving existing configuration, treating as empty"),
        #{};
      Conf -> Conf
    end,
  update_conf(ExistingOverrideConf, NewComponentConf).

decode_conf(Conf) when is_binary(Conf) ->
  case catch jiffy:decode(Conf, [return_maps, dedupe_keys, {null_term, undefined}]) of
    DecodedConf when is_map(DecodedConf) -> DecodedConf;
    Err -> {error, Err}
  end.

update_conf(ExistingOverrideConf, NewComponentConf) ->
  ExistingConf = ?NORMALIZE_MAP(ExistingOverrideConf),
  NewConf = ?NORMALIZE_MAP(NewComponentConf),

  try update_plugin_conf(NewConf)
  catch
    PluginUpdateErr -> logger:warning("Unable to update plugin configuration: ~p", [PluginUpdateErr])
  end,

  NewOverrideConf = maps:get(?KEY_EMQX_CONFIG, NewConf),
  logger:debug("Updating emqx override config. existing=~p, override=~p", [ExistingConf, NewOverrideConf]),
  try update_override_conf(ExistingConf, NewOverrideConf)
  catch
    OverrideUpdateErr -> logger:warning("Unable to update emqx override configuration: ~p", [OverrideUpdateErr])
  end.

%%--------------------------------------------------------------------
%% Update Plugin Config
%%--------------------------------------------------------------------

%% TODO trigger actions on config change
update_plugin_conf(#{} = NewComponentConf) when map_size(NewComponentConf) == 0 ->
  clear_plugin_conf();
update_plugin_conf(NewComponentConf) ->
  CheckedConf = validate_plugin_conf(NewComponentConf),
  update_plugin_env(CheckedConf).

clear_plugin_conf() ->
  application:set_env(?ENV_APP, ?KEY_AUTH_MODE, ?DEFAULT_AUTH_MODE),
  application:set_env(?ENV_APP, ?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, ?DEFAULT_USE_GREENGRASS_MANAGED_CERTIFICATES).

validate_plugin_conf(Conf) ->
  try
    {_, CheckedConf} = hocon_tconf:map_translate(gg_schema, #{?SCHEMA_ROOT => Conf}, #{return_plain => true, format => map}),
    maps:get(?SCHEMA_ROOT, ?NORMALIZE_MAP(CheckedConf))
  catch throw:E:ST ->
    {error, {config_validation, E, ST}}
  end.

update_plugin_env(ComponentConf) ->
  %% TODO find a more dynamic way
  AuthMode = maps:get(?KEY_AUTH_MODE, ComponentConf, ?DEFAULT_AUTH_MODE),
  application:set_env(?ENV_APP, ?KEY_AUTH_MODE, AuthMode),
  logger:info("Updated ~p plugin config to ~p", [[?ENV_APP, ?KEY_AUTH_MODE], AuthMode]),
  UseGreengrassManagedCertificates = maps:get(?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, ComponentConf, ?DEFAULT_USE_GREENGRASS_MANAGED_CERTIFICATES),
  application:set_env(?ENV_APP, ?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, UseGreengrassManagedCertificates),
  logger:info("Updated ~p plugin config to ~p", [[?ENV_APP, ?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES], UseGreengrassManagedCertificates]),
  ConfigUpdateMode = maps:get(?KEY_CONFIG_UPDATE_MODE, ComponentConf, ?DEFAULT_CONFIG_UPDATE_MODE),
  application:set_env(?ENV_APP, ?KEY_CONFIG_UPDATE_MODE, ConfigUpdateMode),
  logger:info("Updated ~p plugin config to ~p", [[?ENV_APP, ?KEY_CONFIG_UPDATE_MODE], ConfigUpdateMode]).

%%--------------------------------------------------------------------
%% Update EMQX Override Config
%%--------------------------------------------------------------------

update_override_conf(ExistingConf, NewConf) ->
  update_override_conf(config_update_mode(), ExistingConf, NewConf).

update_override_conf(merge, _, #{} = NewConf) when map_size(NewConf) == 0 ->
  ok;
update_override_conf(override, ExistingConf, #{} = NewConf) when map_size(NewConf) == 0 ->
  clear_override_conf(ExistingConf);

update_override_conf(merge, ExistingConf, NewConf) ->
  MergedConf = hocon:deep_merge(ExistingConf, NewConf),
  perform_if_different(MergedConf, ExistingConf, fun() -> do_update_override_conf(MergedConf, maps:keys(MergedConf)) end);
update_override_conf(override, ExistingConf, NewConf) ->
  perform_if_different(ExistingConf, NewConf,
    fun() ->
      MapToClear = maps:filter(fun(K,_) -> not maps:is_key(K, NewConf) end, ExistingConf),
      clear_override_conf(MapToClear),
      do_update_override_conf(NewConf, maps:keys(NewConf))
    end
  ).

perform_if_different(OldMap, NewMap, Action) ->
  case emqx_utils_maps:diff_maps(OldMap, NewMap) of
    #{added := Added, removed := Removed, changed := Updated} when
      map_size(Added) =/= 0,
      map_size(Removed) =/= 0,
      map_size(Updated) =/= 0
    -> Action();
    _ -> skip
  end.

do_update_override_conf(_, []) ->
  ok;
do_update_override_conf(Conf, [Key | Rest]) ->
  ConfPath = [Key],
  case catch emqx_conf:update(ConfPath, maps:get(Key, Conf), ?CONF_OPTS) of
    {ok, _} -> logger:info("Updated ~p config", [ConfPath]);
    {error, UpdateError} -> logger:warning("Failed to update configuration. confPath=~p, error=~p", [ConfPath, UpdateError]);
    Err -> logger:warning("Failed to update configuration. confPath=~p, error=~p", [ConfPath, Err])
  end,
  do_update_override_conf(Conf, Rest).

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
