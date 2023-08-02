%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(gg_conf).

-export([auth_mode/0, use_greengrass_managed_certificates/0]).

-export([start/0, stop/0]).
-export([receive_conf_updates/0, do_receive_conf_updates/0, request_update/0, request_update_sync/0]).
-export([register_config_change_handler/2]).

-type(auth_mode() :: enabled | bypass_on_failure | bypass).
-type(use_greengrass_managed_certificates() :: true | false).

-export_type([auth_mode/0, use_greengrass_managed_certificates/0]).

-define(CONF_TIMEOUT_MILLIS, 30000).

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

-define(KEY_CONFIG_CHANGE_HANDLER, config_change_handler).

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
%% On Config Change Callbacks
%%--------------------------------------------------------------------

register_config_change_handler(Path, Handler) ->
  Handlers = application:get_env(?ENV_APP, ?KEY_CONFIG_CHANGE_HANDLER, #{}),
  application:set_env(?ENV_APP, ?KEY_CONFIG_CHANGE_HANDLER, Handlers#{Path => Handler}).

get_config_change_handler(Path) ->
  Handlers = application:get_env(?ENV_APP, ?KEY_CONFIG_CHANGE_HANDLER, #{}),
  maps:get(Path, Handlers, undefined).

fire_config_change(Path, NewVal) ->
  case get_config_change_handler(Path) of
    undefined -> skip;
    Handler -> Handler(NewVal)
  end.

%%--------------------------------------------------------------------
%% Greengrass EMQX Defaults
%%--------------------------------------------------------------------

greengrass_emqx_default_conf() ->
  application:get_env(?ENV_APP, ?KEY_DEFAULT_EMQX_CONF, #{}).

load_greengrass_emqx_default_conf() ->
  %% we know this is valid conf because it's also used in emqx.conf
  Conf0 = case hocon:load(emqx:etc_file("gg.emqx.conf")) of
               {ok, C} ->
                 normalize_map(C);
               {error, Err} -> exit({error, {unable_to_read_config, Err}})
             end,
  %% also include env overrides because we set some in the recipe (e.g. ssl keyfile/certfile)
  Conf1 = hocon_tconf:merge_env_overrides(emqx_schema, Conf0, all, #{format => map}),
  %% some fields are lost during the override merge, bring them back in
  Conf2 = hocon:deep_merge(Conf0, Conf1),
  application:set_env(?ENV_APP, ?KEY_DEFAULT_EMQX_CONF, Conf2).

%%--------------------------------------------------------------------
%% Config Update Listener
%%--------------------------------------------------------------------

%% Subscribe to configuration updates and perform a one-time configuration update.
start() ->
  case load_greengrass_emqx_default_conf() of
    ok -> ok;
    Err -> exit(Err)
  end,
  receive_conf_updates(),
  gg_port_driver:subscribe_to_configuration_updates(fun request_update/0),
  case request_update_sync() of
    ok -> ok;
    Error -> exit(Error)
  end.

stop() ->
  ?UPDATE_PROC ! stop.

receive_conf_updates() ->
  ListenPID = spawn(?MODULE, do_receive_conf_updates, []),
  register(?UPDATE_PROC, ListenPID).

do_receive_conf_updates() ->
  receive
    {update, Pid} ->
      logger:info("Config update request received"),
      case update_conf_from_ipc() of
        ok -> Pid ! ?CONF_UPDATE;
        Err -> logger:error("Unable to get configuration, err=~p", [Err])
      end,
      do_receive_conf_updates();
    stop -> ok
  end.

request_update_sync() ->
  request_update(),
  wait_for_config_update(?CONF_TIMEOUT_MILLIS).

request_update() ->
  logger:info("Requesting configuration update"),
  ?UPDATE_PROC ! {update, self()}.

wait_for_config_update(Timeout) ->
  receive
    ?CONF_UPDATE -> ok
  after Timeout ->
    {error, "Timed out waiting for configuration"}
  end.

%%--------------------------------------------------------------------
%% IPC Config Update Handler
%%--------------------------------------------------------------------

update_conf_from_ipc() ->
  case gg_port_driver:get_configuration() of
  {ok, Conf} ->
    update_conf(Conf),
    ok;
  {error, _} = Error -> Error
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
  logger:debug("Updating emqx override config. existing=~p, override=~p", [ExistingConf, OverrideConf]),
  case catch update_override_conf(ExistingConf, OverrideConf) of
    %% TODO optimize, we don't want to restart every time, ideally, but this is the most defensive against edge cases.
    %% restart listener. otherwise, in some cases, ssl_server_session_cache_db no_longer_defined error occurs
    ok -> gg_listeners:restart_default_ssl_listener();
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

update_plugin_conf(PluginConf) ->
  update_plugin_conf(PluginConf, ?KEY_AUTH_MODE, ?DEFAULT_AUTH_MODE),
  update_plugin_conf(PluginConf, ?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, ?DEFAULT_USE_GREENGRASS_MANAGED_CERTIFICATES).

update_plugin_conf(PluginConf, Key, Default) ->
  PrevVal = application:get_env(?ENV_APP, Key, undefined),
  NewVal = maps:get(Key, PluginConf, Default),
  if PrevVal =/= NewVal ->
    application:set_env(?ENV_APP, Key, NewVal),
    logger:info("Updated ~p plugin config to ~p", [Key, NewVal]),
    fire_config_change(Key, NewVal);
    true -> skip
  end.

%%--------------------------------------------------------------------
%% Update EMQX Override Config
%%--------------------------------------------------------------------

get_override_conf(Conf) ->
  PluginConf = get_plugin_conf(Conf),
  Conf1 = maps:get(?KEY_EMQX_CONFIG, Conf, #{}),
  Conf2 = hocon:deep_merge(greengrass_emqx_default_conf(), Conf1),
  Conf3 = maps:filter(fun(K,_) -> not maps:is_key(K, PluginConf) end, Conf2),
  Conf4 = maps:filter(fun(Key, _) -> not lists:member(Key, ?READONLY_KEYS) end, Conf3),
  Conf5 = no_cacertfile_workaround(Conf4),
  Conf5.

%% We don't set cacertfile in ssl options. In order for this to take
%% in emqx_conf_cli:load_config, we must remove it from the configuration map.
%% Setting cacertfile to null, undefined, or empty string does not work (as of EMQX 5.1.1)
no_cacertfile_workaround(#{<<"listeners">> := Val} = Conf) ->
  Conf#{<<"listeners">> => no_cacertfile_workaround(Val)};
no_cacertfile_workaround(#{<<"ssl">> := Val} = Conf) ->
  Conf#{<<"ssl">> => no_cacertfile_workaround(Val)};
no_cacertfile_workaround(#{<<"default">> := Val} = Conf) ->
  Conf#{<<"default">> => no_cacertfile_workaround(Val)};
no_cacertfile_workaround(#{<<"ssl_options">> := #{<<"cacertfile">> := CA} = Val} = Conf) when CA == null; CA == undefined ->
  Conf#{<<"ssl_options">> => maps:remove(<<"cacertfile">>, Val)};
no_cacertfile_workaround(Conf)->
  Conf.

update_override_conf(ExistingConf, #{} = NewConf) when map_size(NewConf) == 0 ->
  clear_override_conf(ExistingConf);
update_override_conf(ExistingConf, NewConf) ->
  MapToClear = maps:filter(fun(K,_) -> not maps:is_key(K, NewConf) end, ExistingConf),
  clear_override_conf(MapToClear),
  emqx_conf_cli:load_config(emqx_utils_json:encode(NewConf), replace).

clear_override_conf(Conf) when is_map(Conf) ->
  ConfPaths0 = leaf_config_paths(Conf),
  ConfPaths1 = uniq(ConfPaths0),
  lists:foreach(fun remove_override_conf/1, ConfPaths1).

remove_override_conf([]) ->
  ok;
remove_override_conf([_]) -> %% EMQX doesn't allow root key removal
  ok;
remove_override_conf(Path) when is_list(Path) ->
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
  lists:map(fun ({Key, Val}) -> lists:flatten([Key] ++ leaf_config_paths(Val)) end, maps:to_list(Conf));
leaf_config_paths(_) ->
  [].

normalize_map(Map) ->
  Map1 = emqx_utils_maps:binary_key_map(Map), %% configs like authentication only work with binary keys
  Map2 = replace_value(Map1, fun(V) -> V == null end, undefined), %% null is okay in emqx.conf but apparently not during an update
  Map2.

replace_value(Element, Predicate, NewVal) when is_map(Element) ->
  maps:fold(fun(K,V,Acc) -> maps:put(K, replace_value(V, Predicate, NewVal), Acc) end, #{}, Element);
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
