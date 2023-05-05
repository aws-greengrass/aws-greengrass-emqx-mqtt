%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_listeners).

-export([get_listener_config/2, set_verify_fun/2, restart_listener/3]).


-spec(set_verify_fun(#{}, function()) -> #{}).
set_verify_fun(ListenerConf, CustomVerifyFun) ->
  SslOpts = proplists:get_value(ssl_options, ListenerConf),
  NewSslOpts = set_verify_fun_on_ssl_opts(SslOpts, CustomVerifyFun),
  NewListenerConf = replace(ListenerConf, ssl_options, NewSslOpts),
  NewListenerConf.

-spec(set_verify_fun_on_ssl_opts(SslOpts :: list() | undefined, CustomVerifyFun :: function()) -> list()).
set_verify_fun_on_ssl_opts(SslOpts, CustomVerifyFun) when SslOpts =:= undefined ->
  set_verify_fun_on_ssl_opts([], CustomVerifyFun);
set_verify_fun_on_ssl_opts(SslOpts, CustomVerifyFun) ->
  lists:append(SslOpts, [{verify_fun, {CustomVerifyFun, []}}]).

replace(Opts, Key, Value) -> [{Key, Value} | proplists:delete(Key, Opts)].

-spec(get_listener_config(list(), atom, string) -> #{} | listener_not_found).
get_listener_config([{ProtoName, #{ListenerName := Listener}}| _], Proto, Name)
  when Name =:= ListenerName, Proto =:= ProtoName -> Listener;
get_listener_config(_, _, _) -> listener_not_found.

-spec(get_listener_config(atom, string) -> #{} | listener_not_found).
get_listener_config(Proto, Name) ->
  get_listener_config(maps:to_list(emqx:get_config([listeners], #{})), Proto, Name).

-spec(restart_listener(atom, string, #{}) -> ok | {error, any()}).
restart_listener(Proto, Name, Config) ->
  emqx_listeners:restart_listener(Proto, Name, Config).
