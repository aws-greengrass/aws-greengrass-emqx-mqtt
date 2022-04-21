%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_auth).

-include("emqx.hrl").

-import(port_driver_integration,[on_client_authenticate/1]).

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

on_client_connect(ConnInfo = #{clientid := ClientId}, Props, _Env) ->
    logger:debug("Client(~s) connect, ConnInfo: ~n~p~n, Props: ~n~p~n",
              [ClientId, ConnInfo, Props]),
    {ok, Props}.

on_client_connected(ClientInfo = #{clientid := ClientId}, ConnInfo, _Env) ->
    logger:debug("Client(~s) connected, ClientInfo:~n~p~n, ConnInfo:~n~p~n",
              [ClientId, ClientInfo, ConnInfo]).

on_client_disconnected(ClientInfo = #{clientid := ClientId}, ReasonCode, ConnInfo, _Env) ->
    logger:debug("Client(~s) disconnected due to ~p, ClientInfo:~n~p~n, ConnInfo:~n~p~n",
              [ClientId, ReasonCode, ClientInfo, ConnInfo]).

on_client_authenticate(ClientInfo = #{clientid := ClientId}, Result, _Env) ->
    logger:debug("Client(~s) authenticate, ClientInfo ~n~p~n, Result:~n~p~n",
              [ClientId, ClientInfo, Result]),
    case port_driver_integration:on_client_authenticate(ClientId) of
        {ok, <<1>>} ->
            logger:debug("Client(~s) authenticated successfully", [ClientId]),
            {ok, Result#{auth_result => success}};
        {ok, Res} ->
            logger:debug("Client(~s) not authenticated. Res:~p", [ClientId, Res]),
            {stop, Result#{auth_result => not_authorized}};
        {error, Error} ->
            logger:debug("Client(~s) not authenticated. Error:~p", [ClientId, Error]),
            {stop, Result#{auth_result => not_authorized}}
    end.

on_client_check_acl(ClientInfo = #{clientid := ClientId}, Topic, PubSub, Result, _Env) ->
    logger:debug("Client(~s) check_acl, PubSub:~p, Topic:~p, ClientInfo ~n~p~n; Result:~n~p~n",
              [ClientId, PubSub, Topic, ClientInfo, Result]),
    {ok, Result}.

%%--------------------------------------------------------------------
%% Called when the plugin application stop
%%--------------------------------------------------------------------

unload() ->
    emqx:unhook('client.connect',      {?MODULE, on_client_connect}),
    emqx:unhook('client.connected',    {?MODULE, on_client_connected}),
    emqx:unhook('client.disconnected', {?MODULE, on_client_disconnected}),
    emqx:unhook('client.authenticate', {?MODULE, on_client_authenticate}),
    emqx:unhook('client.check_acl',    {?MODULE, on_client_check_acl}).

