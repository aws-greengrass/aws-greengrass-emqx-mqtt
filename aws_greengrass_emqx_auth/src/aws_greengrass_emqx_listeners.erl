%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_listeners).

-compile({no_auto_import,[map_get/2]}).
-export([put_verify_fun/3]).

%% Set verify_fun erlang ssl option on a listener.
%% Listener is restarted for changes to take effect.
%% NOTE: restart may not be needed eventually, see
%%       https://github.com/emqx/emqx/discussions/7695#discussioncomment-2618206
-spec(put_verify_fun(atom, atom, function()) -> ok | {error, any()}).
put_verify_fun(Proto, Name, VerifyFun) ->
  case get_listener_config(Proto, Name) of
    listener_not_found -> {error, listener_not_found};
    Conf -> put_verify_fun(Conf, Proto, Name, VerifyFun)
  end.

-spec(put_verify_fun(#{}, atom, atom, function()) -> ok | {error, any()}).
put_verify_fun(#{ssl_options := SslOpts} = Conf, Proto, Name, VerifyFun) ->
  case conf_update_listener_verify_fun(Proto, Name, VerifyFun) of
    error -> {error, unable_to_update_config};
    ok ->
      NewSslOpts = maps:put(verify_fun, {VerifyFun, []}, SslOpts),
      NewListenerConf = maps:put(ssl_options, NewSslOpts, Conf),
      emqx_listeners:restart_listener(Proto, Name, NewListenerConf)
  end;
put_verify_fun(_, _, _, _) ->
  listener_missing_ssl_options.

-spec(conf_update_listener_verify_fun(atom, atom, function()) -> ok | error).
conf_update_listener_verify_fun(Proto, Name, VerifyFun) ->
  case emqx_conf:update(
    [listeners, Proto, Name, ssl_options, verify_fun],
    {VerifyFun, []},
    #{rawconf_with_defaults => true, override_to => cluster}
  ) of
    {ok, _} -> ok;
    {error, Err} ->
      logger:debug("Unable to update ~p:~p listener emqx conf with verify_fun: ~p", [Proto, Name, Err]),
      error
  end.

-spec(get_listener_config(list(), atom, atom) -> #{} | listener_not_found).
get_listener_config([{ProtoName, Listeners}| _], Proto, Name) when Proto =:= ProtoName ->
  case map_get(Name, Listeners) of
    undefined ->
      logger:debug("listener ~p not found. listeners=~p", [Name, maps:keys(Listeners)]),
      listener_not_found;
    Conf -> Conf
  end;
get_listener_config(_, _, _) -> listener_not_found.

-spec(get_listener_config(atom, atom) -> #{} | listener_not_found).
get_listener_config(Proto, Name) ->
  get_listener_config(maps:to_list(emqx:get_config([listeners], #{})), Proto, Name).

-spec(map_get(atom, #{}) -> undefined | any()).
map_get(K, Map) ->
  case catch maps:get(K, Map) of
    {'EXIT', _} -> undefined;
    V -> V
  end.
