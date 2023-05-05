%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_listeners).

-export([get_listener_config/2, put_verify_fun/2, has_verify_fun/1, restart_listener/3]).

-spec(put_verify_fun(#{}, function()) -> #{}).
put_verify_fun(ListenerConf, CustomVerifyFun) ->
  SslOpts = map_get(ssl_options, ListenerConf),
  NewSslOpts = do_put_verify_fun(SslOpts, CustomVerifyFun),
  NewListenerConf = maps:put(ssl_options, NewSslOpts, ListenerConf),
  NewListenerConf.

-spec(do_put_verify_fun(SslOpts :: #{} | undefined, CustomVerifyFun :: function()) -> list()).
do_put_verify_fun(SslOpts, CustomVerifyFun) when SslOpts =:= undefined ->
  do_put_verify_fun([], CustomVerifyFun);
do_put_verify_fun(SslOpts, CustomVerifyFun) ->
  maps:put(verify_fun, {CustomVerifyFun, []}, SslOpts).

-spec(has_verify_fun(#{}) -> boolean()).
has_verify_fun(ListenerConf) ->
  case maps:find(verify_fun, ListenerConf) of
    {ok, Fun} when is_function(Fun) -> true;
    _ -> false
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

-spec(restart_listener(atom, atom, #{}) -> ok | {error, any()}).
restart_listener(Proto, Name, Config) ->
  emqx_listeners:restart_listener(Proto, Name, Config).

-spec(map_get(atom, #{}) -> undefined | any()).
map_get(K, Map) ->
  case catch maps:get(K, Map) of
    {'EXIT', _} -> undefined;
    V -> V
  end.
