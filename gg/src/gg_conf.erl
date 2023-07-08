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
      Conf = normalize_map(C),
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
%% and update EMQX override configuration with the result.
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
  ExistingConf = normalize_map(ExistingOverrideConf),
  NewConf = normalize_map(NewComponentConf),

  PluginConf = get_plugin_conf(NewConf),
  try update_plugin_conf(PluginConf)
  catch
    PluginUpdateErr -> logger:warning("Unable to update plugin configuration: ~p", [PluginUpdateErr])
  end,

  OverrideConf = get_override_conf(NewConf),
  %% TODO detect change of non-reloadable conf
  logger:debug("Updating emqx override config. existing=~p, override=~p", [ExistingConf, OverrideConf]),
  case catch update_override_conf(ExistingConf, OverrideConf) of
    ok -> ok;
    OverrideUpdateError -> logger:warning("Unable to update emqx override configuration: ~p", [OverrideUpdateError])
  end.

%%--------------------------------------------------------------------
%% Update Plugin Config
%%--------------------------------------------------------------------

get_plugin_conf(Conf) ->
  %% Unknown fields not allowed when checking schema
  Fields = lists:map(fun(SchemaEntry) -> atom_to_binary(element(1, SchemaEntry)) end, gg_schema:fields(gg_schema:namespace())),
  PluginConf = maps:filter(fun(Key, _) -> lists:member(Key, Fields) end, Conf),
  validate_plugin_conf(PluginConf).

validate_plugin_conf(PluginConf) ->
  try
    {_, CheckedConf} = hocon_tconf:map_translate(gg_schema, #{?SCHEMA_ROOT => PluginConf}, #{return_plain => true, format => map}),
    maps:get(?SCHEMA_ROOT, normalize_map(CheckedConf))
  catch throw:E:ST ->
    {error, {config_validation, E, ST}}
  end.

%% TODO trigger actions on config change
update_plugin_conf(#{} = PluginConf) when map_size(PluginConf) == 0 ->
  application:set_env(?ENV_APP, ?KEY_AUTH_MODE, ?DEFAULT_AUTH_MODE),
  application:set_env(?ENV_APP, ?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, ?DEFAULT_USE_GREENGRASS_MANAGED_CERTIFICATES);
update_plugin_conf(PluginConf) ->
  %% TODO find a more dynamic way
  AuthMode = maps:get(?KEY_AUTH_MODE, PluginConf, ?DEFAULT_AUTH_MODE),
  application:set_env(?ENV_APP, ?KEY_AUTH_MODE, AuthMode),
  logger:info("Updated ~p plugin config to ~p", [[?ENV_APP, ?KEY_AUTH_MODE], AuthMode]),
  UseGreengrassManagedCertificates = maps:get(?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, PluginConf, ?DEFAULT_USE_GREENGRASS_MANAGED_CERTIFICATES),
  application:set_env(?ENV_APP, ?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, UseGreengrassManagedCertificates),
  logger:info("Updated ~p plugin config to ~p", [[?ENV_APP, ?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES], UseGreengrassManagedCertificates]).


%%--------------------------------------------------------------------
%% Update EMQX Override Config
%%--------------------------------------------------------------------

get_override_conf(Conf) ->
  PluginConf = get_plugin_conf(Conf),
  Conf1 = maps:get(?KEY_EMQX_CONFIG, Conf, #{}),
  Conf2 = hocon:deep_merge(greengrass_emqx_default_conf(), Conf1),
  Conf3 = maps:filter(fun(K,_) -> not maps:is_key(K, PluginConf) end, Conf2),
  Conf4 = maps:filter(fun(Key, _) -> not lists:member(Key, ?READONLY_KEYS) end, Conf3),
  Conf4.

update_override_conf(ExistingConf, #{} = NewConf) when map_size(NewConf) == 0 ->
  clear_override_conf(ExistingConf);
update_override_conf(ExistingConf, NewConf) ->
  MapToClear = maps:filter(fun(K,_) -> not maps:is_key(K, NewConf) end, ExistingConf),
  clear_override_conf(MapToClear),
  emqx_conf_cli:load_config(emqx_utils_json:encode(NewConf), replace).

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
  %% remove subpaths as well, EMQX applications may be listening for config changes on a subpath,
  %% so deleting just leaf configs may not trigger these listeners.
  %% for example, bridges only reacts when [bridges, http, <bridge_name>] path changes.
  remove_override_conf(lists:droplast(Path)).

leaf_config_paths(Conf) when is_map(Conf) ->
  lists:flatmap(fun ({Key, Val}) -> Key ++ leaf_config_paths(Val) end, maps:to_list(Conf));
leaf_config_paths(_) ->
  [].

normalize_map(Map) ->
  BinMap = emqx_utils_maps:binary_key_map(Map), %% configs like authentication only work with binary keys
  replace_value(BinMap, fun(V) -> V == null end, undefined). %% null is okay in emqx.conf but apparently not during an update

replace_value(Element, Predicate, NewVal) when is_map(Element) ->
  maps:map(fun(Key, Value) -> {Key, replace_value(Value, Predicate, NewVal)} end, Element);
replace_value(Element, Predicate, NewVal) when is_list(Element) ->
  lists:map(fun(Elem) -> replace_value(Elem, Predicate, NewVal) end, Element);
replace_value(Element, Predicate, NewVal) ->
  case Predicate(Element) of
    true -> NewVal;
    _ -> Element
  end.

%% use lists:uniq instead when we upgrade to OTP 25
uniq([]) ->
  [];
uniq([H|T]) ->
  [H | [X || X <- uniq(T), H /= X]].
