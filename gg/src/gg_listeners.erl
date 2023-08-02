%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(gg_listeners).

-export([restart_default_ssl_listener/0]).

restart_default_ssl_listener() ->
  restart_listener(ssl, default).

restart_listener(Proto = ssl, Name = default) ->
  Conf = case gg_conf:auth_mode() of
    bypass -> get_listener_config(Proto, Name);
    _ -> put_verify_fun(Proto, Name, fun gg_tls:verify_client_certificate/3)
  end,
  restart_listener(Proto, Name, Conf);
restart_listener(Proto, Name) ->
  restart_listener(Proto, Name, get_listener_config(Proto, Name)).
restart_listener(_Proto, _Name, {error, _} = Error) ->
  Error;
restart_listener(Proto, Name, Config) ->
  emqx_listeners:restart_listener(Proto, Name, Config).

-spec(get_listener_config(list(), atom, atom) -> #{} | {error, listener_not_found}).
get_listener_config([{ProtoName, Listeners}| _], Proto, Name) when Proto =:= ProtoName ->
  case maps:get(Name, Listeners, undefined) of
    undefined ->
      logger:debug("listener ~p not found. listeners=~p", [Name, maps:keys(Listeners)]),
      {error, listener_not_found};
    Conf -> Conf
  end;
get_listener_config(_, _, _) -> {error, listener_not_found}.

-spec(get_listener_config(atom, atom) -> #{} | listener_not_found).
get_listener_config(Proto, Name) ->
  get_listener_config(maps:to_list(emqx:get_config([listeners], #{})), Proto, Name).

%% Set verify_fun erlang ssl option on a listener.
%% Listener is restarted for changes to take effect.
%% NOTE: restart may not be needed eventually, see
%%       https://github.com/emqx/emqx/discussions/7695#discussioncomment-2618206
-spec(put_verify_fun(atom, atom, function()) -> #{} | {error, any()}).
put_verify_fun(Proto, Name, VerifyFun) ->
  case get_listener_config(Proto, Name) of
    listener_not_found -> {error, listener_not_found};
    Conf -> put_verify_fun(Conf, VerifyFun)
  end.
put_verify_fun(#{ssl_options := SslOpts} = Conf, VerifyFun) ->
  NewSslOpts = maps:put(verify_fun, {VerifyFun, []}, SslOpts),
  NewListenerConf = maps:put(ssl_options, NewSslOpts, Conf),
  NewListenerConf;
put_verify_fun(_, _) ->
  {error, listener_missing_ssl_options}.
