%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_auth).

-include("emqx.hrl").

-import(port_driver_integration, [get_auth_token/2
, on_client_connect/2
, on_client_connected/2
, on_client_disconnected/2
, on_client_check_acl/4
]).

-export([load/1
  , unload/0
]).

%%--------------------------------------------------------------------
%% Client Lifecycle Hooks
%%--------------------------------------------------------------------

-export([on_client_connect/3
  , on_client_connected/3
  , on_client_disconnected/4
  , on_client_authenticate/3
  , on_client_check_acl/5
]).

%% Called when the plugin application start
load(Env) ->
  emqx:hook('client.connect', {?MODULE, on_client_connect, [Env]}),
  emqx:hook('client.connected', {?MODULE, on_client_connected, [Env]}),
  emqx:hook('client.disconnected', {?MODULE, on_client_disconnected, [Env]}),
  emqx:hook('client.authenticate', {?MODULE, on_client_authenticate, [Env]}),
  emqx:hook('client.check_acl', {?MODULE, on_client_check_acl, [Env]}).

%%--------------------------------------------------------------------
%% Client Lifecycle Hooks Impl
%%--------------------------------------------------------------------

on_client_connect(ConnInfo = #{clientid := ClientId, peercert := PeerCert}, Props, _Env) ->
  logger:debug("Client(~s) connect, ConnInfo: ~n~p~n, Props: ~n~p~n, Env:~n~p~n",
    [ClientId, ConnInfo, Props, _Env]),

  PeerCertEncoded = encode_peer_cert(PeerCert),
  %% Client cert is not available when we get subsequent callbacks (on_client_authenticate, on_client_check_acl)
  %% Putting the value in the erlang process's dictionary
  put(cert_pem, PeerCertEncoded),

  case port_driver_integration:on_client_connect(ClientId, PeerCertEncoded) of
    {ok, pass} -> {ok, Props};
    {ok, fail} -> stop;
    {error, Reason} ->
      logger:error("Client(~s). Failed to call driver. Reason:~p", [ClientId, Reason]),
      stop;
    Other ->
      logger:error("Unknown response ~p", [Other]),
      stop
  end.

on_client_connected(ClientInfo = #{clientid := ClientId}, ConnInfo, _Env) ->
  logger:debug("Client(~s) connected, ClientInfo:~n~p~n, ConnInfo:~n~p~n, Env:~n~p~n",
    [ClientId, ClientInfo, ConnInfo, _Env]),

  PeerCertEncoded = get(cert_pem),
  case port_driver_integration:on_client_connected(ClientId, PeerCertEncoded) of
    {ok, pass} -> ok;
    {ok, fail} -> stop;
    {error, Reason} ->
      logger:error("Client(~s). Failed to call driver. Reason:~p", [ClientId, Reason]),
      stop;
    Other ->
      logger:error("Unknown response ~p", [Other]),
      stop
  end.

on_client_disconnected(ClientInfo = #{clientid := ClientId}, ReasonCode, ConnInfo, _Env) ->
  logger:debug("Client(~s) disconnected due to ~p, ClientInfo:~n~p~n, ConnInfo:~n~p~n, Env:~n~p~n",
    [ClientId, ReasonCode, ClientInfo, ConnInfo, _Env]),

  PeerCertEncoded = get(cert_pem),
  case port_driver_integration:on_client_disconnected(ClientId, PeerCertEncoded) of
    {ok, pass} -> ok;
    {ok, fail} -> stop;
    {error, Reason} ->
      logger:error("Client(~s). Failed to call driver. Reason:~p", [ClientId, Reason]),
      stop;
    Other ->
      logger:error("Unknown response ~p", [Other]),
      stop
  end.

on_client_authenticate(ClientInfo = #{clientid := ClientId}, Result, _Env) ->
  logger:debug("Client(~s) authenticate, ClientInfo ~n~p~n, Result:~n~p~n, Env:~n~p~n",
    [ClientId, ClientInfo, Result, _Env]),

  PeerCertEncoded = get(cert_pem),
  case get_auth_token_for_client(ClientId, PeerCertEncoded) of
    {ok, AuthToken} ->
      logger:info("Client(~s) is valid", [ClientId]),
      %% puts authToken in the process dictionary
      %% to be retrieved during authorization check
      put(cda_auth_token, AuthToken),
      {ok, Result#{auth_result => success}};
    {error, Error} ->
      logger:error("Client(~s) not authenticated. Error:~p", [ClientId, Error]),
      {stop, Result#{auth_result => not_authorized}};
    Other ->
      logger:error("Unknown response ~p", [Other]),
      {stop, Result#{auth_result => not_authorized}}
  end.

%% Retrives authToken from CDA if not stored in process dictionary
get_auth_token_for_client(ClientId, CertPem) ->
  case get(cda_auth_token) of
    undefined -> port_driver_integration:get_auth_token(ClientId, CertPem);
    AuthToken -> {ok, AuthToken}
  end.

on_client_check_acl(ClientInfo = #{clientid := ClientId}, PubSub, Topic, Result, _Env) ->
  logger:debug("Client(~s) check_acl, PubSub:~p, Topic:~p, ClientInfo ~n~p~n; Result:~n~p~n, Env: ~n~p~n",
    [ClientId, PubSub, Topic, ClientInfo, Result, _Env]),

  AuthToken = get(cda_auth_token),
  case port_driver_integration:on_client_check_acl(ClientId, AuthToken, Topic, PubSub) of
    {ok, authorized} ->
      logger:info("Client(~s) authorized to perform ~p on topic ~p", [ClientId, PubSub, Topic]),
      {stop, allow};
    {ok, unauthorized} ->
      logger:warn("Client(~s) not authorized to perform ~p on topic ~p", [ClientId, PubSub, Topic]),
      {stop, deny};
    {error, Error} ->
      logger:error("Client(~s) not authorized to perform ~p on topic ~p. Error:~p",
        [ClientId, PubSub, Topic, Error]),
      {stop, deny}
  end.

encode_peer_cert(PeerCert) ->
  case PeerCert of
    nossl ->
      <<"">>;
    undefined ->
      <<"">>;
    _ ->
      base64:encode(PeerCert)
  end.

%%--------------------------------------------------------------------
%% Called when the plugin application stop
%%--------------------------------------------------------------------

unload() ->
  emqx:unhook('client.connect', {?MODULE, on_client_connect}),
  emqx:unhook('client.connected', {?MODULE, on_client_connected}),
  emqx:unhook('client.disconnected', {?MODULE, on_client_disconnected}),
  emqx:unhook('client.authenticate', {?MODULE, on_client_authenticate}),
  emqx:unhook('client.check_acl', {?MODULE, on_client_check_acl}).
