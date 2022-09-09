%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_listeners).

-export([update_ssl_listener_options/1, start_updated_ssl_listener/1]).


-spec(update_ssl_listener_options(function()) -> emqx_listeners:listener() | nossl).
update_ssl_listener_options(CustomVerifyFun) ->
  case find_ssl_listener(emqx:get_env(listeners, [])) of
    nossl -> nossl;
    SslListener ->
      NewListenerOpts = add_custom_verify_to_ssl_options(maps:get('opts', SslListener, []), CustomVerifyFun),
      maps:put('opts', NewListenerOpts, SslListener)
  end.

-spec(add_custom_verify_to_ssl_options(Options :: list(), CustomVerifyFun :: function()) -> list()).
add_custom_verify_to_ssl_options(Options, CustomVerifyFun) ->
  case proplists:get_value(ssl_options, Options) of
    undefined -> Options;
    SslOpts ->
      NewSslOpts = lists:append(SslOpts,
        [{verify_fun,
          {
            CustomVerifyFun, []
          }
        }]),
      replace(Options, ssl_options, NewSslOpts)
  end.

-spec(start_updated_ssl_listener(emqx_listeners:listener()) -> ok | {error, any()}).
start_updated_ssl_listener(UpdatedSslListener) ->
  case restart_ssl_listener(UpdatedSslListener) of
    ok ->
      logger:info("Restarted ~p ~w listener on port ~w with custom certificate verification",
        [maps:get(name, UpdatedSslListener),
          maps:get(proto, UpdatedSslListener),
          maps:get(listen_on, UpdatedSslListener)]),
      update_ssl_listener_env(UpdatedSslListener),
      ok;
    {error, Reason} ->
      logger:error("Failed to restart ~p ~w listener on port ~w with custom certificate verification: ~p",
        [maps:get(name, UpdatedSslListener),
          maps:get(proto, UpdatedSslListener),
          maps:get(listen_on, UpdatedSslListener),
          Reason]),
      {error, Reason}
  end.

-spec(restart_ssl_listener(emqx_listeners:listener()) -> ok | {error, any()}).
restart_ssl_listener(UpdatedSslListener) ->
  case stop_existing_ssl_listener() of
    ok -> start_listener(UpdatedSslListener);
    {error, Reason} -> {error, Reason}
  end.

-spec(stop_existing_ssl_listener() -> ok | {error, any()}).
stop_existing_ssl_listener() ->
  case find_ssl_listener(emqx:get_env(listeners, [])) of
    nossl -> ok;
    SslListener -> emqx_listeners:stop_listener(SslListener)
  end.

-spec(start_listener(emqx_listeners:listener()) -> ok | {error, any()}).
start_listener(UpdatedSslListener) ->
  case emqx_listeners:start_listener(UpdatedSslListener) of
    ok -> ok;
    Error -> {error, Error}
  end.

-spec(find_ssl_listener(Listeners :: list()) -> emqx_listeners:listener() | nossl).
find_ssl_listener([]) -> nossl;
find_ssl_listener([#{name := "external", proto := 'ssl'} = L | _]) -> L;
find_ssl_listener([_ | Rest]) -> find_ssl_listener(Rest).

-spec(update_ssl_listener_env(emqx_listeners:listener()) -> ok).
update_ssl_listener_env(SslListener) ->
  emqx_listeners:update_listeners_env('update', SslListener).

replace(Opts, Key, Value) -> [{Key, Value} | proplists:delete(Key, Opts)].
