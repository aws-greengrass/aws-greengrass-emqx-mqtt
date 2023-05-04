%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_listeners).

-export([find_listener/2, set_verify_fun/2, restart_listener/1, debug_listeners/0]).


-spec(set_verify_fun(emqx_listeners:listener(), function()) -> emqx_listeners:listener()).
set_verify_fun(Listener, CustomVerifyFun) ->
  Opts = get_opts(Listener),
  SslOpts = get_ssl_opts(Opts),
  NewSslOpts = set_verify_fun_on_ssl_opts(SslOpts, CustomVerifyFun),
  NewOpts = replace(Opts, ssl_options, NewSslOpts),
  NewListener = replace(Listener, opts, NewOpts),
  NewListener.

-spec(get_ssl_opts(list()) -> undefined | list()).
get_ssl_opts(Opts) ->
  proplists:get_value(ssl_options, Opts).

-spec(get_opts(emqx_listeners:listener()) -> list()).
get_opts(Listener) ->
  maps:get('opts', Listener, []).

-spec(set_verify_fun_on_ssl_opts(SslOpts :: list() | undefined, CustomVerifyFun :: function()) -> list()).
set_verify_fun_on_ssl_opts(SslOpts, CustomVerifyFun) when SslOpts =:= undefined ->
  set_verify_fun_on_ssl_opts([], CustomVerifyFun);
set_verify_fun_on_ssl_opts(SslOpts, CustomVerifyFun) ->
  lists:append(SslOpts, [{verify_fun, {CustomVerifyFun, []}}]).

replace(Opts, Key, Value) -> [{Key, Value} | proplists:delete(Key, Opts)].

-spec(find_listener(Listeners :: list(), atom, string) -> emqx_listeners:listener() | listener_not_found).
find_listener([], _, _) -> listener_not_found;
find_listener([#{name := ListenerName, proto := ProtoName} = L | _], Proto, Name)
  when Name =:= ListenerName, Proto =:= ProtoName -> L;
find_listener([_ | Rest], Proto, Name) -> find_listener(Rest, Proto, Name).
-spec(find_listener(atom, string) -> emqx_listeners:listener() | listener_not_found).
find_listener(Proto, Name) ->
  find_listener(application:get_env(emqx, listeners, []), Proto, Name).

debug_listeners() ->
  logger:info("listeners from env: ~p", [application:get_env(emqx, listeners, [])]),
  logger:info("listeners from env list: ~p", [maps:to_list(emqx:get_config([listeners], #{}))]),
  logger:info("listeners from api: ~p", [emqx_listeners:list()]).

-spec(restart_listener(emqx_listeners:listener()) -> ok | {error, any()}).
restart_listener(Listener) ->
  case emqx_listeners:stop_listener(Listener) of
    ok -> start_listener(Listener);
    {error, Err} -> {error, Err}
  end.

-spec(start_listener(emqx_listeners:listener()) -> ok | {error, any()}).
start_listener(#{proto := Proto, listen_on := ListenOn, opts := Options} = Listener) ->
  case emqx_listeners:start_listener(Proto, ListenOn, Options) of
    {ok, _} ->
      emqx_listeners:update_listeners_env('update', Listener),
      ok;
    {error, Err} -> {error, Err}
  end.
