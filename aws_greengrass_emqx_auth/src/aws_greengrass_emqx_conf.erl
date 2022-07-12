%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_conf).

-export([auth_mode/0, use_greengrass_managed_certificates/0]).
-export([load/0]).

-type(auth_mode() :: enabled | bypass_on_failure | bypass).
-type(use_greengrass_managed_certificates() :: true | false).

-export_type([auth_mode/0, use_greengrass_managed_certificates/0]).

-define(CONF_FILE_NAME, "aws_greengrass_emqx_auth.hocon").
-define(ENV_APP, aws_greengrass_emqx_auth).

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

%% Read plugin configuration from file and load it into the application environment.
%% The config file is expected to reside in the plugin's priv dir, and the file is
%% in hocon format.
-spec(load() -> ok | {error, any()}).
load() ->
  case read_config() of
    {error, _} = Err -> Err;
    Conf -> set_env(Conf)
  end.

-spec(read_config() -> hocon:config() | {error, any()}).
read_config() ->
  case code:priv_dir(?ENV_APP) of
    {error, Err} -> {error, {plugin_dir_not_found, Err}};
    PluginConfDir ->
      ConfFile = filename:join([PluginConfDir, ?CONF_FILE_NAME]),
      read_config(ConfFile)
  end.

-spec(read_config(file:filename()) -> hocon:config() | {error, any()}).
read_config(ConfFile) ->
  Conf = case hocon:load(ConfFile) of
    {ok, C} -> C;
    {error, Err} -> {error, {unable_to_read_config, Err}}
  end,
  try
    {_, CheckedConf} =
      hocon_tconf:map_translate(aws_greengrass_emqx_auth_schema, Conf, #{return_plain => true, format => map}),
    emqx_map_lib:unsafe_atom_key_map(CheckedConf)
  catch
    throw:E:ST ->
      {error, {config_validation, E, ST}}
  end.

-spec(set_env(hocon:config()) -> ok).
set_env(Conf) ->
  logger:debug("Plugin Configuration: ~p", [Conf]),
  %% TODO find a more dynamic way
  RootConfig = maps:get(aws_greengrass_emqx_auth, Conf),
  AuthMode = maps:get(?KEY_AUTH_MODE, RootConfig, ?DEFAULT_AUTH_MODE),
  application:set_env(?ENV_APP, ?KEY_AUTH_MODE, AuthMode),
  UseGreengrassManagedCertificates = maps:get(?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, RootConfig, ?DEFAULT_USE_GREENGRASS_MANAGED_CERTIFICATES),
  application:set_env(?ENV_APP, ?KEY_USE_GREENGRASS_MANAGED_CERTIFICATES, UseGreengrassManagedCertificates).
