%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_auth).

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_hooks.hrl").


%% keys for data stored in process dict
-define(PEER_ENCODED_CERT, cert_pem).
-define(CLIENT_MQTT_VERSION, client_version).
-define(AUTH_TOKEN, cda_auth_token).

-define(CONTINUE_HOOK_CHAIN, ok).
-define(STOP_HOOK_CHAIN, stop).

%% part of return value in authN hook
-define(AUTHN_SUCCESS, success).
-define(AUTHN_FAILURE, not_authorized).

%% internal auth success/failure
-define(AUTHORIZED, authorized).
-define(UNAUTHORIZED, unauthorized).

%% part of return value in authZ hook
-define(AUTHZ_ALLOW, allow).
-define(AUTHZ_DENY, deny).

-export([load/1, unload/0]).
-export([on_client_connect/3, on_client_authenticate/3, on_client_authorize/5]).

%%--------------------------------------------------------------------
%% Client Lifecycle Hooks
%%--------------------------------------------------------------------

load(Env) ->
  hook('client.connect', {?MODULE, on_client_connect, [Env]}),
  hook('client.authenticate', {?MODULE, on_client_authenticate, [Env]}),
  hook('client.authorize', {?MODULE, on_client_authorize, [Env]}).

unload() ->
  unhook('client.connect', {?MODULE, on_client_connect}),
  unhook('client.authenticate', {?MODULE, on_client_authenticate}),
  unhook('client.authorize', {?MODULE, on_client_authorize}).

hook(HookPoint, MFA) ->
  %% use highest hook priority so this module's callbacks
  %% are evaluated before the default hooks in EMQX
  emqx_hooks:add(HookPoint, MFA, _Property = ?HP_HIGHEST).

unhook(HookPoint, MFA) ->
  emqx_hooks:del(HookPoint, MFA).

-spec(execute_auth_hook(function()) -> ?CONTINUE_HOOK_CHAIN | {?STOP_HOOK_CHAIN, {error, any()} | #{result => ?AUTHZ_DENY}}).
execute_auth_hook(Hook) ->
  case catch Hook() of
    Result -> handle_auth_hook_result(aws_greengrass_emqx_conf:auth_mode(), Result)
  end.

handle_auth_hook_result(_, {ok, _} = AuthNSuccess) ->
  %% no need to evaluate rest of auth chain when we deem success
  {?STOP_HOOK_CHAIN, AuthNSuccess};
handle_auth_hook_result(_, #{result := ?AUTHZ_ALLOW} = AuthZSuccess) ->
  %% no need to evaluate rest of auth chain when we deem success
  {?STOP_HOOK_CHAIN, AuthZSuccess};
handle_auth_hook_result(AuthMode, _) when AuthMode =:= bypass; AuthMode =:= bypass_on_failure ->
  %% in bypass mode, ignore auth results/errors and move on to next step in auth chain
  ?CONTINUE_HOOK_CHAIN;
handle_auth_hook_result(_, #{result := ?AUTHZ_DENY} = AuthZDeny) ->
  %% stop auth chain and report authZ deny
  {?STOP_HOOK_CHAIN, AuthZDeny};
handle_auth_hook_result(_, {error, _} = Error) ->
  %% stop auth chain and report error
  {?STOP_HOOK_CHAIN, Error};
handle_auth_hook_result(_, Error) ->
  %% stop auth chain and report error
  {?STOP_HOOK_CHAIN, {error, Error}}.

%%--------------------------------------------------------------------
%% Connect Hook
%%--------------------------------------------------------------------

on_client_connect(ConnInfo, Props, _Env) ->
  handle_connect(ConnInfo, Props, _Env).

handle_connect(ConnInfo, Props, _Env) ->
  handle_connect(aws_greengrass_emqx_conf:auth_mode(), ConnInfo, Props, _Env).

handle_connect(bypass, _, Props, _) ->
  {?CONTINUE_HOOK_CHAIN, Props};
handle_connect(_, ConnInfo = #{clientid := ClientId, peercert := PeerCert, proto_ver := ClientVersion}, Props, _Env) ->
  logger:debug("Client(~s) connect, ConnInfo: ~n~p~n, Props: ~n~p~n, Env:~n~p~n", [ClientId, ConnInfo, Props, _Env]),
  %% required for authN
  put(?PEER_ENCODED_CERT, encode_peer_cert(PeerCert)),
  %% used for informational purposes
  put(?CLIENT_MQTT_VERSION, ClientVersion),
  {?CONTINUE_HOOK_CHAIN, Props}.

%%--------------------------------------------------------------------
%% AuthN Hook
%%--------------------------------------------------------------------

%% Interface derived from
%% https://github.com/emqx/emqx/blob/270059f0c2694342fc72338760dbb968b78b7918/apps/emqx/src/emqx_access_control.erl#L53-L68
on_client_authenticate(ClientInfo = #{clientid := ClientId}, Result, _Env) ->
  execute_auth_hook(
    fun() ->
      logger:debug("Client(~s) authenticate, ClientInfo ~n~p~n, Result:~n~p~n, Env:~n~p~n", [ClientId, ClientInfo, Result, _Env]),
      authenticate(ClientId)
    end
  ).

-spec(authenticate(ClientId :: any()) -> {ok, #{result := ?AUTHN_SUCCESS}} | {error, ?AUTHN_FAILURE}).
authenticate(ClientId) ->
  authenticate(get(?AUTH_TOKEN), ClientId, get(?PEER_ENCODED_CERT)).

-spec(authenticate(AuthToken :: any() | {error, _}, ClientId :: any(), CertPem :: any()) -> {ok, any()} | {error, any()}).
authenticate(undefined, ClientId, CertPem) ->
  authenticate(port_driver_integration:get_auth_token(ClientId, CertPem), ClientId, CertPem);
authenticate({error, Err}, ClientId, _) ->
  logger:error("Client(~s) not authenticated. Error:~p", [ClientId, Err]),
  {error, ?AUTHN_FAILURE};
authenticate({ok, AuthToken}, ClientId, CertPem) ->
  authenticate(AuthToken, ClientId, CertPem);
authenticate(AuthToken, ClientId, _) ->
  %% store for authZ
  put(?AUTH_TOKEN, AuthToken),
  case is_connect_authorized(ClientId) of
    true -> {ok, #{result => ?AUTHN_SUCCESS}};
    false -> {error, ?AUTHN_FAILURE}
  end.

-spec(reauthenticate(ClientId :: any()) -> {ok, #{result := ?AUTHN_SUCCESS}} | {error, ?AUTHN_FAILURE}).
reauthenticate(ClientId) ->
  %% clear auth token before getting a new one
  erase(?AUTH_TOKEN),
  logger:info("Attempting to get new auth token."),
  authenticate(ClientId).

%%--------------------------------------------------------------------
%% AuthZ Hook
%%--------------------------------------------------------------------

%% Interface derived from
%% https://github.com/emqx/emqx/blob/270059f0c2694342fc72338760dbb968b78b7918/apps/emqx/src/emqx_access_control.erl#L121-L127
on_client_authorize(ClientInfo = #{clientid := ClientId}, PubSub, Topic, Result, _Env) ->
  execute_auth_hook(
    fun() ->
      logger:debug("Client(~s) check_acl, PubSub:~p, Topic:~p, ClientInfo ~n~p~n; Result:~n~p~n, Env: ~n~p~n",
        [ClientId, PubSub, Topic, ClientInfo, Result, _Env]),
      case is_pubsub_authorized(PubSub, ClientId, Topic) of
        true -> #{result => ?AUTHZ_ALLOW};
        false -> #{result => ?AUTHZ_DENY}
      end
    end
  ).

-spec(is_pubsub_authorized(PubSub :: atom(), ClientId :: any(), Topic :: any()) -> boolean()).
is_pubsub_authorized(publish, ClientId, Topic) ->
  is_publish_authorized(ClientId, Topic);
is_pubsub_authorized(subscribe, ClientId, Topic) ->
  is_subscribe_authorized(ClientId, Topic).

-spec(is_connect_authorized(ClientId :: any()) -> boolean()).
is_connect_authorized(ClientId) ->
  Resource = "mqtt:clientId:" ++ binary_to_list(ClientId),
  Action = "mqtt:connect",
  case is_authorized(ClientId, Resource, Action) of
    ?AUTHORIZED -> true;
    _ -> false
  end.

-spec(is_publish_authorized(ClientId :: any(), Topic :: any()) -> boolean()).
is_publish_authorized(ClientId, Topic) ->
  Resource = "mqtt:topic:" ++ binary_to_list(Topic),
  Action = "mqtt:publish",
  case is_authorized(ClientId, Resource, Action) of
    ?AUTHORIZED -> true;
    _ -> false
  end.

-spec(is_subscribe_authorized(ClientId :: any(), Topic :: any()) -> boolean()).
is_subscribe_authorized(ClientId, Topic) ->
  Resource = "mqtt:topicfilter:" ++ binary_to_list(Topic),
  Action = "mqtt:subscribe",
  case is_authorized(ClientId, Resource, Action) of
    ?AUTHORIZED -> true;
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

is_authorized({ok, authorized}, _, _, _, _, _) ->
  ?AUTHORIZED;
is_authorized({ok, unauthorized}, _, _, _, _, _) ->
  ?UNAUTHORIZED;
is_authorized({error, Error}, _, _, ClientId, Resource, Action) ->
  logger:debug("Client(~s) not authorized to perform ~p on resource ~p. Error:~p", [ClientId, Action, Resource, Error]),
  ?UNAUTHORIZED;
is_authorized({ok, bad_token}, Retries, _, ClientId, Resource, Action) when Retries == 0 ->
  logger:debug("Client(~s) has a bad auth token. EMQX will try to get a new auth token from client device auth component.", [ClientId]),
  case reauthenticate(ClientId) of
    {ok, _} -> is_authorized(Retries + 1, ClientId, Resource, Action);
    _ ->
      logger:info("Could not get a new auth token. Kicking client (~s) to have client reconnect with updated credentials", [ClientId]),
      emqx_mgmt:kickout_client(ClientId),
      ?UNAUTHORIZED
  end;
is_authorized({ok, bad_token}, Retries, _, ClientId, Resource, Action) when Retries > 0 ->
  logger:debug("Retry attempt failed. Client(~s) not authorized to perform ~p on resource ~p. Error: Could not get valid auth token.", [ClientId, Action, Resource]),
  kick_non_v5_client(ClientId),
  ?UNAUTHORIZED.


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
