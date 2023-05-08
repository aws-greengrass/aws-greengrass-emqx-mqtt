%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_auth).

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_hooks.hrl").

-define(AUTH_CHAIN_PASSTHROUGH, ok).
-define(PEER_ENCODED_CERT, cert_pem).
-define(CLIENT_MQTT_VERSION, client_version).
-define(AUTH_TOKEN, cda_auth_token).

-export([load/1, unload/0]).
-export([on_client_connect/3, on_client_authenticate/3, on_client_check_acl/5]).

%%--------------------------------------------------------------------
%% Client Lifecycle Hooks
%%--------------------------------------------------------------------

load(Env) ->
  hook('client.connect', {?MODULE, on_client_connect, [Env]}),
  hook('client.authenticate', {?MODULE, on_client_authenticate, [Env]}),
  hook('client.check_acl', {?MODULE, on_client_check_acl, [Env]}).

unload() ->
  unhook('client.connect', {?MODULE, on_client_connect}),
  unhook('client.authenticate', {?MODULE, on_client_authenticate}),
  unhook('client.check_acl', {?MODULE, on_client_check_acl}).

hook(HookPoint, MFA) ->
  %% use highest hook priority so this module's callbacks
  %% are evaluated before the default hooks in EMQX
  emqx_hooks:add(HookPoint, MFA, _Property = ?HP_HIGHEST).

unhook(HookPoint, MFA) ->
  emqx_hooks:del(HookPoint, MFA).

execute_auth_hook(Hook) ->
  execute_auth_hook(aws_greengrass_emqx_conf:auth_mode(), Hook).

execute_auth_hook(enabled, Hook) ->
  Hook();
execute_auth_hook(bypass, _) ->
  ?AUTH_CHAIN_PASSTHROUGH;
execute_auth_hook(bypass_on_failure, Hook) ->
  case catch Hook() of
    {_, success} = AuthNPass -> AuthNPass;
    {_, allow} = AuthZPass -> AuthZPass;
    _ -> ?AUTH_CHAIN_PASSTHROUGH
  end.

%%--------------------------------------------------------------------
%% Connect Hook
%%--------------------------------------------------------------------

on_client_connect(ConnInfo, Props, _Env) ->
  handle_connect(ConnInfo, Props, _Env).

handle_connect(ConnInfo, Props, _Env) ->
  handle_connect(aws_greengrass_emqx_conf:auth_mode(), ConnInfo, Props, _Env).

handle_connect(bypass, _, Props, _) ->
  {ok, Props};
handle_connect(_, ConnInfo = #{clientid := ClientId, peercert := PeerCert, proto_ver := ClientVersion}, Props, _Env) ->
  logger:debug("Client(~s) connect, ConnInfo: ~n~p~n, Props: ~n~p~n, Env:~n~p~n", [ClientId, ConnInfo, Props, _Env]),
  %% Use erlang process dictionary so data is available between callbacks
  put(?PEER_ENCODED_CERT, encode_peer_cert(PeerCert)),
  put(?CLIENT_MQTT_VERSION, ClientVersion),
  {ok, Props}.

%%--------------------------------------------------------------------
%% AuthN Hook
%%--------------------------------------------------------------------

on_client_authenticate(ClientInfo = #{clientid := ClientId}, Result, _Env) ->
  execute_auth_hook(
    fun() ->
      logger:debug("Client(~s) authenticate, ClientInfo ~n~p~n, Result:~n~p~n, Env:~n~p~n", [ClientId, ClientInfo, Result, _Env]),
      authenticate(ClientId)
    end
  ).

-spec(authenticate(ClientId :: any()) -> {ok, any()} | {error, any()}).
authenticate(ClientId) ->
  authenticate(get(?AUTH_TOKEN), ClientId, get(?PEER_ENCODED_CERT)).

-spec(authenticate(AuthToken :: any() | {error, _}, ClientId :: any(), CertPem :: any()) -> {ok, any()} | {error, any()}).
authenticate(undefined, ClientId, CertPem) ->
  authenticate(port_driver_integration:get_auth_token(ClientId, CertPem), ClientId, CertPem);
authenticate({error, Err}, ClientId, _) ->
  logger:error("Client(~s) not authenticated. Error:~p", [ClientId, Err]),
  {error, not_authorized};
authenticate({ok, AuthToken}, ClientId, CertPem) ->
  authenticate(AuthToken, ClientId, CertPem);
authenticate(AuthToken, ClientId, _) ->
  logger:info("Client(~s) is valid", [ClientId]),
  %% store for authZ
  put(?AUTH_TOKEN, AuthToken),
  case is_connect_authorized(ClientId) of
    true -> {ok, success};
    false -> {stop, not_authorized}
  end.

reauthenticate(ClientId) ->
  %% clear auth token before getting a new one
  erase(?AUTH_TOKEN),
  logger:info("Attempting to get new auth token."),
  authenticate(ClientId).

%%--------------------------------------------------------------------
%% AuthZ Hook
%%--------------------------------------------------------------------

on_client_check_acl(ClientInfo = #{clientid := ClientId}, PubSub, Topic, Result, _Env) ->
  execute_auth_hook(
    fun() ->
      logger:debug("Client(~s) check_acl, PubSub:~p, Topic:~p, ClientInfo ~n~p~n; Result:~n~p~n, Env: ~n~p~n",
        [ClientId, PubSub, Topic, ClientInfo, Result, _Env]),
      case is_pubsub_authorized(PubSub, ClientId, Topic) of
        true -> {stop, allow};
        false -> {stop, deny}
      end
    end
  ).

is_pubsub_authorized(publish, ClientId, Topic) ->
  is_publish_authorized(ClientId, Topic);
is_pubsub_authorized(subscribe, ClientId, Topic) ->
  is_subscribe_authorized(ClientId, Topic).

is_connect_authorized(ClientId) ->
  Resource = "mqtt:clientId:" ++ binary_to_list(ClientId),
  Action = "mqtt:connect",
  case is_authorized(ClientId, Resource, Action) of
    authorized -> true;
    _ -> false
  end.

is_publish_authorized(ClientId, Topic) ->
  Resource = "mqtt:topic:" ++ binary_to_list(Topic),
  Action = "mqtt:publish",
  case is_authorized(ClientId, Resource, Action) of
    authorized -> true;
    _ -> false
  end.

is_subscribe_authorized(ClientId, Topic) ->
  Resource = "mqtt:topicfilter:" ++ binary_to_list(Topic),
  Action = "mqtt:subscribe",
  case is_authorized(ClientId, Resource, Action) of
    authorized -> true;
    _ -> false
  end.


-spec(is_authorized(ClientId :: any(), Resource :: string, Action :: string) -> authorized | unauthorized).
is_authorized(ClientId, Resource, Action) ->
  is_authorized(0, ClientId, Resource, Action).

is_authorized(Retries, ClientId, Resource, Action) ->
  %% Use auth token from AuthN hook
  is_authorized(Retries, get(?AUTH_TOKEN), ClientId, Resource, Action).

is_authorized(Retries, AuthToken, ClientId, Resource, Action) ->
  is_authorized(port_driver_integration:on_client_check_acl(ClientId, AuthToken, Resource, Action), Retries, AuthToken, ClientId, Resource, Action).

is_authorized({ok, authorized}, _, _, ClientId, Resource, Action) ->
  logger:info("Client(~s) authorized to perform ~p on resource ~p", [ClientId, Action, Resource]),
  authorized;
is_authorized({ok, unauthorized}, _, _, ClientId, Resource, Action) ->
  logger:warning("Client(~s) not authorized to perform ~p on resource ~p", [ClientId, Action, Resource]),
  unauthorized;
is_authorized({error, Error}, _, _, ClientId, Resource, Action) ->
  logger:error("Client(~s) not authorized to perform ~p on resource ~p. Error:~p", [ClientId, Action, Resource, Error]),
  unauthorized;
is_authorized({ok, bad_token}, Retries, _, ClientId, Resource, Action) when Retries == 0 ->
  logger:warning("Client(~s) has a bad auth token. EMQX will try to get a new auth token from client device auth component.", [ClientId]),
  case reauthenticate(ClientId) of
    {_, success} -> is_authorized(Retries + 1, ClientId, Resource, Action);
    _ ->
      logger:info("Could not get a new auth token"),
      unauthorized
  end;
is_authorized({ok, bad_token}, Retries, _, ClientId, Resource, Action) when Retries > 0 ->
  logger:error("Retry attempt failed. Client(~s) not authorized to perform ~p on resource ~p. Error: Could not get valid auth token.", [ClientId, Action, Resource]),
  kick_non_v5_client(ClientId),
  unauthorized.


%%--------------------------------------------------------------------
%% Utils
%%--------------------------------------------------------------------

kick_non_v5_client(ClientId) ->
  kick_non_v5_client(get(?CLIENT_MQTT_VERSION), ClientId).

kick_non_v5_client(Version, ClientId) when Version < 5 ->
  logger:info("Disconnecting MQTTv3 client(~s).", [ClientId]),
  emqx_mgmt:kickout_client(ClientId);
kick_non_v5_client(Version, ClientId) when Version /= 5 ->
  logger:info("Client(~s) has an unknown MQTT version ~p. Disconnecting client", [ClientId, Version]),
  emqx_mgmt:kickout_client(ClientId);
kick_non_v5_client(_, _) ->
  ok.

encode_peer_cert(nossl) ->
  <<"">>;
encode_peer_cert(undefined) ->
  <<"">>;
encode_peer_cert(PeerCert) ->
  base64:encode(PeerCert).
