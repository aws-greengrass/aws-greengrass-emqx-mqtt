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

%% TODO figure out how to import emqx_conf.hrl instead
-define(READONLY_KEYS, [<<"cluster">>, <<"rpc">>, <<"node">>]).

%% config keys
-define(KEY_EMQX_CONFIG, <<"emqxConfig">>).
-define(KEY_AUTH_MODE, <<"authMode">>).
-define(KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, <<"useGreengrassManagedCertificates">>).
%% defaults
-define(DEFAULT_AUTH_MODE, enabled).
-define(DEFAULT_USE_GREENGRASS_MANAGED_CERTIFICATES, true).

-define(SCHEMA_ROOT, list_to_binary(gg_schema:namespace())).

-define(ENV_APP, aws_greengrass_emqx_auth).
-define(KEY_DEFAULT_EMQX_CONF, default_emqx_conf).

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

%%--------------------------------------------------------------------
%% Greengrass EMQX Defaults
%%--------------------------------------------------------------------

greengrass_emqx_default_conf() ->
  application:get_env(?ENV_APP, ?KEY_DEFAULT_EMQX_CONF, #{}).

load_greengrass_emqx_default_conf() ->
  %% TODO include environment variables e.g. certfile
  %% we know this is valid conf because it's also used in emqx.conf
  case hocon:load(emqx:etc_file("gg.emqx.conf")) of
    {ok, C} ->
      Conf = ?NORMALIZE_MAP(C),
      application:set_env(?ENV_APP, ?KEY_DEFAULT_EMQX_CONF, Conf),
      ok;
    {error, Err} -> {error, {unable_to_read_config, Err}}
  end.

%%--------------------------------------------------------------------
%% Config Update Listener
%%--------------------------------------------------------------------

%% Subscribe to configuration updates and perform a one time configuration update.
start() ->
  case load_greengrass_emqx_default_conf() of
    ok -> ok;
    Err -> exit(Err)
  end,
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
%% IPC Config Update Handler
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
    case emqx_config:read_override_conf(?CONF_OPTS) of
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

  NewOverrideConf = hocon:deep_merge(greengrass_emqx_default_conf(), maps:get(?KEY_EMQX_CONFIG, NewConf, #{})),
  ReloadableConf = maps:filter(fun(Key, _) -> not lists:member(Key, ?READONLY_KEYS) end, NewOverrideConf),
  %% TODO detect change of non-reloadable conf
  logger:debug("Updating emqx override config. existing=~p, override=~p", [ExistingConf, ReloadableConf]),
  case update_override_conf(ExistingConf, ReloadableConf) of
    ok -> ok;
    OverrideUpdateError -> logger:warning("Unable to update emqx override configuration: ~p", [OverrideUpdateError])
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
    %% Unknown fields not allowed when checking schema
    Fields = lists:map(fun(SchemaEntry) -> element(1, SchemaEntry) end, gg_schema:fields(gg_schema:namespace())),
    PluginConf = maps:filter(fun(Key, _) -> lists:member(Key, Fields) end, Conf),
    {_, CheckedConf} = hocon_tconf:map_translate(gg_schema, #{?SCHEMA_ROOT => PluginConf}, #{return_plain => true, format => map}),
      %% TODO remove
    logger:debug("schemaFields=~p, conf=~p, pluginConf=~p, checkedConf=~p", [Fields, Conf, PluginConf, CheckedConf]),
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
  logger:info("Updated ~p plugin config to ~p", [[?ENV_APP, ?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES], UseGreengrassManagedCertificates]).


%%--------------------------------------------------------------------
%% Update EMQX Override Config
%%--------------------------------------------------------------------

update_override_conf(ExistingConf, #{} = NewConf) when map_size(NewConf) == 0 ->
  clear_override_conf(ExistingConf);
update_override_conf(ExistingConf, NewConf) ->
  MapToClear = maps:filter(fun(K,_) -> not maps:is_key(K, NewConf) end, ExistingConf),
  clear_override_conf(MapToClear),
  maps:foreach(fun do_update_override_conf/2, NewConf).

do_update_override_conf(Key, Value) ->
  case catch emqx_conf:update([Key], Value, ?CONF_OPTS) of
    {ok, _} -> logger:info("Updated ~p config", [Key]);
    {error, UpdateError} -> logger:warning("Failed to update configuration. conf=~p, error=~p", [Key, UpdateError]);
    Err -> logger:warning("Failed to update configuration. conf=~p, error=~p", [Key, Err])
  end.

clear_override_conf(Conf) when is_map(Conf) ->
  lists:foreach(fun remove_override_conf/1, uniq(leaf_config_paths(Conf))).

remove_override_conf([]) ->
  ok;
remove_override_conf([_]) -> %% EMQX doesn't allow root key removal
  ok;
remove_override_conf(Path) ->
  case catch emqx_conf:remove(Path, ?CONF_OPTS) of
    {ok, _} -> logger:info("Removed ~p config", [Path]);
    {error, RemoveError} -> logger:warning("Failed to remove configuration. confPath=~p, error=~p", [Path, RemoveError]);
    Err -> logger:warning("Failed to remove configuration. confPath=~p, error=~p", [Path, Err])
  end,
  %% remove subpaths as well, emqx applications may be listening for config changes on a subpath,
  %% so deleting just leaf configs may not trigger these listeners.
  %% for example, bridges only reacts when [bridges, http, <bridge_name>] path changes.
  remove_override_conf(lists:droplast(Path)).

leaf_config_paths(Conf) when is_map(Conf) ->
  lists:flatmap(fun ({Key, Val}) -> Key ++ leaf_config_paths(Val) end, maps:to_list(Conf));
leaf_config_paths(_) ->
  [].

%% use lists:uniq instead when we upgrade to OTP 25
uniq([]) ->
  [];
uniq([H|T]) ->
  [H | [X || X <- uniq(T), H /= X]].
