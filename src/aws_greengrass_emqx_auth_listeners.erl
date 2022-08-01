%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------
-module(aws_greengrass_emqx_auth_listeners).

%% API
-export([get_ssl_listener/0]).
-export([get_opts/1, get_ssl_opts/1, apply_opts/2, apply_ssl_opts/2]).
-export([set_custom_verify_fun/2, remove_custom_verify_fun/2]).
-export([restart_listener/1]).


%% Retrieve the emqx ssl listener, if it exists.
-spec(get_ssl_listener() -> emqx_listeners:listener() | false).
get_ssl_listener() ->
  emqx_listeners:find_by_id(emqx_listeners:identifier(#{proto => ssl, name => list_to_binary("external")})).

%% Retrieve a list of options from a a listener.
-spec(get_opts(Listener :: emqx_listeners:listener()) -> list()).
get_opts(Listener) ->
  maps:get('opts', Listener, []).

%% Retrieve list of ssl options from a list of options or from a listener.
-spec(get_ssl_opts(list() | emqx_listeners:listener()) -> list()).
get_ssl_opts(Opts) when is_list(Opts) ->
  proplists:get_value(ssl_options, Opts, []);
get_ssl_opts(Listener) ->
  get_ssl_opts(get_opts(Listener)).

%% Set a custom verify function, for either a list of ssl options or a listener.
-spec(set_custom_verify_fun(list() | emqx_listeners:listener(), function()) -> list() | emqx_listeners:listener()).
set_custom_verify_fun([{verify_fun, _} | SslOpts], Fun) ->
  [{verify_fun, {Fun, []}}] ++ SslOpts;
set_custom_verify_fun(SslOpts, Fun) when is_list(SslOpts) ->
  [{verify_fun, {Fun, []}}] ++ SslOpts;
set_custom_verify_fun(Listener, Fun) ->
  UpdatedSslOpts = set_custom_verify_fun(get_ssl_opts(Listener), Fun),
  apply_ssl_opts(Listener, UpdatedSslOpts).

%% Remove the custom verify function, for either a list of ssl options or a listener.
-spec(remove_custom_verify_fun(list() | emqx_listeners:listener(), function()) -> list()).
remove_custom_verify_fun([{verify_fun, {CurrFun, _}} | SslOpts], Fun) when is_function(Fun) and CurrFun =:= Fun ->
  remove_custom_verify_fun(SslOpts, Fun);
remove_custom_verify_fun(SslOpts, Fun) when is_function(Fun)->
  SslOpts;
remove_custom_verify_fun(Listener, Fun) when is_function(Fun) ->
  UpdatedSslOpts = remove_custom_verify_fun(get_ssl_opts(Listener), Fun),
  apply_ssl_opts(Listener, UpdatedSslOpts).

%% Return a new listener config with updated options.
-spec(apply_opts(emqx_listeners:listener(), list()) -> emqx_listeners:listener()).
apply_opts(Listener, NewOpts) ->
  maps:put('opts', NewOpts, Listener).

%% Update ssl options for either a list of options or a listener.
-spec(apply_ssl_opts(Listener, NewSslOpts :: list()) -> list() when
  Listener :: emqx_listeners:listener();
    (Opts :: list, NewSslOpts :: list) -> list()).
apply_ssl_opts([{ssl_options, _} | Opts], NewSslOpts) ->
  [{ssl_options, NewSslOpts} | Opts];
apply_ssl_opts(Opts, NewSslOpts) when is_list(Opts) ->
  [{ssl_options, NewSslOpts} | Opts];
apply_ssl_opts(Listener, NewSslOpts) ->
  NewOpts = apply_ssl_opts(get_opts(Listener), NewSslOpts),
  apply_opts(Listener, NewOpts).

%% Restart listener using the provided listener config.
%%
%% NOTE: emqx env will be updated with the provided listener config on successful restart.
-spec(restart_listener(Listener :: emqx_listeners:listener()) -> ok | {error, term()}).
restart_listener(Listener) ->
  case emqx_listeners:stop_listener(Listener) of
    ok -> case catch emqx_listeners:start_listener(Listener) of
            {'EXIT', {Reason, _}} -> {error, Reason};
            ok -> emqx_listeners:update_listeners_env('update', Listener)
          end;
    Err -> Err
  end.
