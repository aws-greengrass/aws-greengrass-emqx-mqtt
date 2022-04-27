%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_auth).

-include("emqx.hrl").

-import(base64, [encode/1]).

-import(port_driver_integration,[ on_client_authenticate/1
        , on_client_connect/2
        , on_client_connected/2
        , on_client_disconnected/2
        , on_client_check_acl/3
        ]).

-export([ load/1
        , unload/0
        ]).

%%--------------------------------------------------------------------
%% Client Lifecycle Hooks
%%--------------------------------------------------------------------

-export([ on_client_connect/3
        , on_client_connected/3
        , on_client_disconnected/4
        , on_client_authenticate/3
        , on_client_check_acl/5
        ]).

%% Called when the plugin application start
load(Env) ->
    emqx:hook('client.connect',      {?MODULE, on_client_connect, [Env]}),
    emqx:hook('client.connected',    {?MODULE, on_client_connected, [Env]}),
    emqx:hook('client.disconnected', {?MODULE, on_client_disconnected, [Env]}),
    emqx:hook('client.authenticate', {?MODULE, on_client_authenticate, [Env]}),
    emqx:hook('client.check_acl',    {?MODULE, on_client_check_acl, [Env]}).

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
        {ok, <<1>>} -> ok;
        Unexpected ->
            logger:error("Client(~s). Failed to call driver. Unexpected:~p", [ClientId, Unexpected])
    end,
    {ok, Props}.

on_client_connected(ClientInfo = #{clientid := ClientId}, ConnInfo, _Env) ->
    logger:debug("Client(~s) connected, ClientInfo:~n~p~n, ConnInfo:~n~p~n, Env:~n~p~n",
              [ClientId, ClientInfo, ConnInfo, _Env]),

    PeerCertEncoded = get(cert_pem),
    case port_driver_integration:on_client_connected(ClientId, PeerCertEncoded) of
        {ok, <<1>>} -> ok;
        Unexpected ->
            logger:error("Client(~s). Failed to call driver. Unexpected:~p", [ClientId, Unexpected])
    end.

on_client_disconnected(ClientInfo = #{clientid := ClientId}, ReasonCode, ConnInfo, _Env) ->
    logger:debug("Client(~s) disconnected due to ~p, ClientInfo:~n~p~n, ConnInfo:~n~p~n, Env:~n~p~n",
              [ClientId, ReasonCode, ClientInfo, ConnInfo, _Env]),

    PeerCertEncoded = get(cert_pem),
    case port_driver_integration:on_client_disconnected(ClientId, PeerCertEncoded) of
        {ok, <<1>>} -> ok;
        Unexpected ->
            logger:error("Client(~s). Failed to call driver. Error:~p", [ClientId, Unexpected])
    end.

on_client_authenticate(ClientInfo = #{clientid := ClientId}, Result, _Env) ->
    logger:debug("Client(~s) authenticate, ClientInfo ~n~p~n, Result:~n~p~n, Env:~n~p~n",
              [ClientId, ClientInfo, Result, _Env]),

    PeerCertEncoded = get(cert_pem),
    case port_driver_integration:on_client_authenticate(ClientId, PeerCertEncoded) of
        {ok, <<1>>} ->
            logger:info("Client(~s) authenticated successfully", [ClientId]),
            {ok, Result#{auth_result => success}};
        {ok, Res} ->
            logger:warn("Client(~s) not authenticated. Res:~p", [ClientId, Res]),
            {stop, Result#{auth_result => not_authorized}};
        {error, Error} ->
            logger:error("Client(~s) not authenticated. Error:~p", [ClientId, Error]),
            {stop, Result#{auth_result => not_authorized}}
    end.

on_client_check_acl(ClientInfo = #{clientid := ClientId}, PubSub, Topic, Result, _Env) ->
    logger:debug("Client(~s) check_acl, PubSub:~p, Topic:~p, ClientInfo ~n~p~n; Result:~n~p~n, Env: ~n~p~n",
              [ClientId, PubSub, Topic, ClientInfo, Result, _Env]),

    PeerCertEncoded = get(cert_pem),
    case port_driver_integration:on_client_check_acl(ClientId, PeerCertEncoded, Topic, PubSub) of
        {ok, <<1>>} ->
            logger:info("Client(~s) authorized to perform ~p on topic ~p", [ClientId, PubSub, Topic]),
            {stop, allow};
        {ok, Res} ->
            logger:warn("Client(~s) not authorized to perform ~p on topic ~p. Res:~p", [ClientId, PubSub, Topic, Res]),
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
    emqx:unhook('client.connect',      {?MODULE, on_client_connect}),
    emqx:unhook('client.connected',    {?MODULE, on_client_connected}),
    emqx:unhook('client.disconnected', {?MODULE, on_client_disconnected}),
    emqx:unhook('client.authenticate', {?MODULE, on_client_authenticate}),
    emqx:unhook('client.check_acl',    {?MODULE, on_client_check_acl}).
