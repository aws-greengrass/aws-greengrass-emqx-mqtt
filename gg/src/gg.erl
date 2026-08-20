%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(gg).

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_hooks.hrl").


%% keys for data stored in process dict
-define(PEER_ENCODED_CERT, cert_pem).
-define(CLIENT_MQTT_VERSION, client_version).
-define(AUTH_TOKEN, cda_auth_token).

-define(CONTINUE_HOOK_CHAIN, ok).
-define(STOP_HOOK_CHAIN, stop).

%% part of return value in authN hook
-define(AUTHN_SUCCESS, success).
-define(AUTHN_FAILURE, not_authorized).

%% internal auth success/failure
-define(AUTHORIZED, authorized).
-define(UNAUTHORIZED, unauthorized).

%% part of return value in authZ hook
-define(AUTHZ_ALLOW, allow).
-define(AUTHZ_DENY, deny).

-export([load/1, unload/0]).
-export([on_client_connect/3, on_client_authenticate/3, on_client_authorize/5]).

%%--------------------------------------------------------------------
%% Client Lifecycle Hooks
%%--------------------------------------------------------------------

load(Env) ->
  hook('client.connect', {?MODULE, on_client_connect, [Env]}),
  hook('client.authenticate', {?MODULE, on_client_authenticate, [Env]}),
  hook('client.authorize', {?MODULE, on_client_authorize, [Env]}),
  log_skipped_listeners().

unload() ->
  unhook('client.connect', {?MODULE, on_client_connect}),
  unhook('client.authenticate', {?MODULE, on_client_authenticate}),
  unhook('client.authorize', {?MODULE, on_client_authorize}).

hook(HookPoint, MFA) ->
  %% use highest hook priority so this module's callbacks
  %% are evaluated before the default hooks in EMQX
  emqx_hooks:add(HookPoint, MFA, _Property = ?HP_HIGHEST).

unhook(HookPoint, MFA) ->
  emqx_hooks:del(HookPoint, MFA).

-spec(execute_auth_hook(function()) -> ?CONTINUE_HOOK_CHAIN | {?STOP_HOOK_CHAIN, {error, any()} | #{result => ?AUTHZ_DENY}}).
execute_auth_hook(Hook) ->
  execute_auth_hook(gg_conf:auth_mode(), Hook).
execute_auth_hook(AuthMode, _Hook) when AuthMode == bypass ->
  ?CONTINUE_HOOK_CHAIN;
execute_auth_hook(AuthMode, Hook) ->
  case catch Hook() of
    Result -> handle_auth_hook_result(AuthMode, Result)
  end.

handle_auth_hook_result(_, {ok, _} = AuthNSuccess) ->
  %% no need to evaluate rest of auth chain when we deem success
  {?STOP_HOOK_CHAIN, AuthNSuccess};
handle_auth_hook_result(_, #{result := ?AUTHZ_ALLOW} = AuthZSuccess) ->
  %% no need to evaluate rest of auth chain when we deem success
  {?STOP_HOOK_CHAIN, AuthZSuccess};
handle_auth_hook_result(AuthMode, Result) when AuthMode =:= bypass; AuthMode =:= bypass_on_failure ->
  %% Defer to the next auth source. Denials are expected here (bypass_on_failure
  %% intentionally falls through to EMQX when GG does not allow); errors and
  %% crashes are not, and this is the mode where they would otherwise go entirely
  %% unreported — GG silently stops deciding and nothing looks wrong.
  log_bypassed_result(Result),
  ?CONTINUE_HOOK_CHAIN;
handle_auth_hook_result(_, #{result := ?AUTHZ_DENY} = AuthZDeny) ->
  %% stop auth chain and report authZ deny
  {?STOP_HOOK_CHAIN, AuthZDeny};
handle_auth_hook_result(_, {error, _} = Error) ->
  %% stop auth chain and report error
  logger:error("GG auth hook returned error, denying: ~p", [Error]),
  {?STOP_HOOK_CHAIN, Error};
handle_auth_hook_result(_, {'EXIT', Reason}) ->
  %% The hook body crashed and execute_auth_hook/2's catch turned it into a
  %% value. Report the cause and location but NOT the failing call's arguments:
  %% a function_clause trace's top frame carries the arg list, and
  %% is_authorized/6 takes the CDA auth token as a positional argument.
  log_hook_crash(Reason),
  {?STOP_HOOK_CHAIN, {error, crashed}};
handle_auth_hook_result(_, Other) ->
  %% stop auth chain and report an otherwise-unrecognized result
  logger:error("GG auth hook returned unexpected result, denying: ~p", [Other]),
  {?STOP_HOOK_CHAIN, {error, Other}}.

%% Report a caught crash by cause and location only. Dropping the stacktrace's
%% argument list keeps the CDA auth token (a positional arg to is_authorized/6)
%% out of the logs — error-level logs are uploaded by log manager and attached
%% to support cases.
log_hook_crash({Reason, [{M, F, _Args, _Loc} | _]}) ->
  logger:error("GG auth hook crashed (~p) in ~p:~p, denying", [Reason, M, F]);
log_hook_crash(Reason) ->
  logger:error("GG auth hook crashed, denying: ~p", [Reason]).

%% In bypass modes GG defers rather than decides. A denial is the ordinary
%% "GG does not authorize, let EMQX decide" outcome and is not worth logging; an
%% error or crash means GG could not decide (e.g. CDA unreachable) and is worth
%% surfacing even though the chain continues.
log_bypassed_result(#{result := ?AUTHZ_DENY}) ->
  ok;
log_bypassed_result({'EXIT', Reason}) ->
  log_hook_crash(Reason);
log_bypassed_result(Result) ->
  logger:warning("GG auth hook failed, bypassing to next auth source: ~p", [Result]).

%%--------------------------------------------------------------------
%% Connect Hook
%%--------------------------------------------------------------------

on_client_connect(ConnInfo, Props, _Env) ->
  handle_connect(ConnInfo, Props, _Env).

handle_connect(ConnInfo, Props, _Env) ->
  handle_connect(gg_conf:auth_mode(), ConnInfo, Props, _Env).

handle_connect(bypass, _, Props, _) ->
  {?CONTINUE_HOOK_CHAIN, Props};
handle_connect(_, ConnInfo = #{clientid := ClientId, peercert := PeerCert, proto_ver := ClientVersion}, Props, _Env) ->
  logger:debug("Client(~s) connect, ConnInfo: ~n~p~n, Props: ~n~p~n, Env:~n~p~n", [ClientId, ConnInfo, Props, _Env]),
  %% required for authN
  put(?PEER_ENCODED_CERT, encode_peer_cert(PeerCert)),
  %% used for informational purposes
  put(?CLIENT_MQTT_VERSION, ClientVersion),
  {?CONTINUE_HOOK_CHAIN, Props}.

%%--------------------------------------------------------------------
%% AuthN Hook
%%--------------------------------------------------------------------

%% Interface derived from
%% https://github.com/emqx/emqx/blob/270059f0c2694342fc72338760dbb968b78b7918/apps/emqx/src/emqx_access_control.erl#L53-L68
%%
%% Listeners with enable_authn: false skip GG auth entirely.
%% EMQX places the listener's enable_authn setting directly in ClientInfo
%% (see emqx_channel:init/2). Defense-in-depth: EMQX also natively skips
%% the authenticate hook chain for these listeners, but we guard here too.
on_client_authenticate(#{enable_authn := false, clientid := ClientId}, _Result, _Env) ->
  logger:debug("Client(~s) skipping GG auth (enable_authn=false)", [ClientId]),
  ?CONTINUE_HOOK_CHAIN;
on_client_authenticate(ClientInfo = #{clientid := ClientId}, Result, _Env) ->
  execute_auth_hook(
    fun() ->
      logger:debug("Client(~s) authenticate, ClientInfo ~n~p~n, Result:~n~p~n, Env:~n~p~n", [ClientId, ClientInfo, Result, _Env]),
      authenticate(ClientId)
    end
  ).

-spec(authenticate(ClientId :: any()) -> {ok, #{result := ?AUTHN_SUCCESS}} | {error, ?AUTHN_FAILURE}).
authenticate(ClientId) ->
  authenticate(get(?AUTH_TOKEN), ClientId, get(?PEER_ENCODED_CERT)).

-spec(authenticate(AuthToken :: any() | {error, _}, ClientId :: any(), CertPem :: any()) -> {ok, any()} | {error, any()}).
authenticate(undefined, ClientId, CertPem) ->
  authenticate(gg_port_driver:get_auth_token(ClientId, CertPem), ClientId, CertPem);
authenticate({error, Err}, ClientId, _) ->
  logger:error("Client(~s) not authenticated. Error:~p", [ClientId, Err]),
  {error, ?AUTHN_FAILURE};
authenticate({ok, AuthToken}, ClientId, CertPem) ->
  authenticate(AuthToken, ClientId, CertPem);
authenticate(AuthToken, ClientId, _) ->
  %% store for authZ
  put(?AUTH_TOKEN, AuthToken),
  case is_connect_authorized(ClientId) of
    true -> {ok, #{result => ?AUTHN_SUCCESS}};
    false -> {error, ?AUTHN_FAILURE}
  end.

-spec(reauthenticate(ClientId :: any()) -> {ok, #{result := ?AUTHN_SUCCESS}} | {error, ?AUTHN_FAILURE}).
reauthenticate(ClientId) ->
  %% clear auth token before getting a new one
  erase(?AUTH_TOKEN),
  logger:info("Attempting to get new auth token."),
  authenticate(ClientId).

%%--------------------------------------------------------------------
%% AuthZ Hook
%%--------------------------------------------------------------------

%% Interface derived from
%% https://github.com/emqx/emqx/blob/270059f0c2694342fc72338760dbb968b78b7918/apps/emqx/src/emqx_access_control.erl#L121-L127
%%
%% Listeners with enable_authn: false skip GG authZ — GG authZ requires
%% the auth token from GG authn, which cannot exist for these clients.
%% Authorization falls through to the EMQX authorization chain (sources + no_match).
on_client_authorize(#{enable_authn := false, clientid := ClientId}, _PubSub, _Topic, _Result, _Env) ->
  logger:debug("Client(~s) skipping GG authZ (enable_authn=false)", [ClientId]),
  ?CONTINUE_HOOK_CHAIN;
on_client_authorize(ClientInfo = #{clientid := ClientId}, PubSub, Topic, Result, _Env) ->
  execute_auth_hook(
    fun() ->
      logger:debug("Client(~s) check_acl, PubSub:~p, Topic:~p, ClientInfo ~n~p~n; Result:~n~p~n, Env: ~n~p~n",
        [ClientId, PubSub, Topic, ClientInfo, Result, _Env]),
      Action = pubsub_action_type(PubSub),
      case is_pubsub_authorized(PubSub, ClientId, Topic) of
        true ->
          %% Allow is the common per-message case; log at debug to avoid one
          %% info line per publish/subscribe on disk-constrained edge devices.
          logger:debug("GG authZ result=allow clientid=~s action=~p topic=~p",
            [ClientId, Action, Topic]),
          #{result => ?AUTHZ_ALLOW};
        false ->
          %% Denials are the security-relevant events worth surfacing at info.
          logger:info("GG authZ result=deny clientid=~s action=~p topic=~p",
            [ClientId, Action, Topic]),
          #{result => ?AUTHZ_DENY}
      end
    end
  ).

%% Extract the action atom from the EMQX 5.1.3 map for logging.
%% Fallback clause ensures logging never crashes authZ.
pubsub_action_type(#{action_type := Action}) -> Action;
pubsub_action_type(Other) -> Other.

%% EMQX 5.1.3 (emqx_types:pubsub()) passes the action as a map
%% #{action_type := publish|subscribe, ...}.
-spec(is_pubsub_authorized(PubSub :: map() | term(), ClientId :: any(), Topic :: any()) -> boolean()).
is_pubsub_authorized(#{action_type := publish}, ClientId, Topic) ->
  is_publish_authorized(ClientId, Topic);
is_pubsub_authorized(#{action_type := subscribe}, ClientId, Topic) ->
  is_subscribe_authorized(ClientId, Topic);
is_pubsub_authorized(Other, ClientId, Topic) ->
  %% Defensive terminal clause. EMQX is pinned in gg/rebar.config, so the two map
  %% clauses above are exhaustive today. But the pin is bumped by routine in-repo
  %% PRs and Erlang gives no compile-time exhaustiveness warning, so a future
  %% reshape of emqx_types:pubsub() would otherwise raise function_clause here —
  %% which the catch in execute_auth_hook/2 turns into deny-everything. Fail
  %% closed (deny) with a named error log instead of a silent crash.
  logger:error("GG authZ unrecognized action shape, denying: clientid=~s action=~p topic=~p",
    [ClientId, Other, Topic]),
  false.

-spec(is_connect_authorized(ClientId :: any()) -> boolean()).
is_connect_authorized(ClientId) ->
  Resource = "mqtt:clientId:" ++ binary_to_list(ClientId),
  Action = "mqtt:connect",
  case is_authorized(ClientId, Resource, Action) of
    ?AUTHORIZED -> true;
    _ -> false
  end.

-spec(is_publish_authorized(ClientId :: any(), Topic :: any()) -> boolean()).
is_publish_authorized(ClientId, Topic) ->
  Resource = "mqtt:topic:" ++ binary_to_list(Topic),
  Action = "mqtt:publish",
  case is_authorized(ClientId, Resource, Action) of
    ?AUTHORIZED -> true;
    _ -> false
  end.

-spec(is_subscribe_authorized(ClientId :: any(), Topic :: any()) -> boolean()).
is_subscribe_authorized(ClientId, Topic) ->
  Resource = "mqtt:topicfilter:" ++ binary_to_list(Topic),
  Action = "mqtt:subscribe",
  case is_authorized(ClientId, Resource, Action) of
    ?AUTHORIZED -> true;
    _ -> false
  end.


-spec(is_authorized(ClientId :: any(), Resource :: string, Action :: string) -> authorized | unauthorized).
is_authorized(ClientId, Resource, Action) ->
  is_authorized(0, ClientId, Resource, Action).

is_authorized(Retries, ClientId, Resource, Action) ->
  %% Use auth token from AuthN hook
  is_authorized(Retries, get(?AUTH_TOKEN), ClientId, Resource, Action).

is_authorized(Retries, AuthToken, ClientId, Resource, Action) ->
  is_authorized(gg_port_driver:on_client_check_acl(ClientId, AuthToken, Resource, Action), Retries, AuthToken, ClientId, Resource, Action).

is_authorized({ok, authorized}, _, _, _, _, _) ->
  ?AUTHORIZED;
is_authorized({ok, unauthorized}, _, _, _, _, _) ->
  ?UNAUTHORIZED;
is_authorized({error, Error}, _, _, ClientId, Resource, Action) ->
  logger:debug("Client(~s) not authorized to perform ~p on resource ~p. Error:~p", [ClientId, Action, Resource, Error]),
  ?UNAUTHORIZED;
is_authorized({ok, bad_token}, Retries, _, ClientId, Resource, Action) when Retries == 0 ->
  logger:debug("Client(~s) has a bad auth token. EMQX will try to get a new auth token from client device auth component.", [ClientId]),
  case reauthenticate(ClientId) of
    {ok, _} -> is_authorized(Retries + 1, ClientId, Resource, Action);
    _ ->
      kick_client(ClientId,
        fun() -> logger:info(
          "Could not get a new auth token.
          Kicking client (~s) to have client reconnect with updated credentials", [ClientId])
        end),
      ?UNAUTHORIZED
  end;
is_authorized({ok, bad_token}, Retries, _, ClientId, Resource, Action) when Retries > 0 ->
  logger:debug("Retry attempt failed. Client(~s) not authorized to perform ~p on resource ~p. Error: Could not get valid auth token.", [ClientId, Action, Resource]),
  kick_non_v5_client(ClientId),
  ?UNAUTHORIZED;
is_authorized(Other, _Retries, _AuthToken, ClientId, Resource, Action) ->
  %% Unrecognized result from the C++ port driver — an external contract across a
  %% language boundary, the same kind that broke with EMQX. Fail closed and log
  %% the result WITHOUT the auth token (_AuthToken) rather than raising
  %% function_clause, whose stacktrace would carry the token as a call argument.
  logger:error("GG authZ unrecognized port driver result, denying: clientid=~s resource=~p action=~p result=~p",
    [ClientId, Resource, Action, Other]),
  ?UNAUTHORIZED.


%%--------------------------------------------------------------------
%% Utils
%%--------------------------------------------------------------------

%% Log once at startup which listeners have GG auth disabled.
log_skipped_listeners() ->
  Listeners = maps:to_list(emqx:get_config([listeners], #{})),
  lists:foreach(
    fun({Proto, NameMap}) ->
      lists:foreach(
        fun({Name, Conf}) ->
          case maps:get(enable_authn, Conf, true) of
            false ->
              logger:warning("GG auth disabled for listener ~p:~p; authorization defers to EMQX sources (no_match=~p)",
                [Proto, Name, emqx:get_config([authorization, no_match], deny)]);
            _ -> ok
          end
        end, maps:to_list(NameMap))
    end, Listeners).

kick_client(ClientId, OnPreKick) ->
  kick_client(gg_conf:auth_mode(), ClientId, OnPreKick).
kick_client(_AuthMode = enabled, ClientId, OnPreKick) ->
  OnPreKick(),
  emqx_mgmt:kickout_client(ClientId);
kick_client(_AuthMode, _ClientId, _OnPreKick) ->
  skip. %% keep connection open so we can fallback to next auth provider in bypass modes

kick_non_v5_client(ClientId) ->
  kick_non_v5_client(get(?CLIENT_MQTT_VERSION), ClientId).

kick_non_v5_client(Version, ClientId) when is_number(Version), Version < 5 ->
  kick_client(ClientId,
    fun() -> logger:info("Disconnecting MQTTv3 client(~s).", [ClientId]) end);
kick_non_v5_client(Version, _ClientId) when is_number(Version), Version =:= 5 ->
  ok;
kick_non_v5_client(Version, ClientId) ->
  kick_client(ClientId,
    fun() ->
      logger:info(
        "Client(~s) has an unknown MQTT version ~p.
        Disconnecting client", [ClientId, Version])
    end).


encode_peer_cert(nossl) ->
  <<"">>;
encode_peer_cert(undefined) ->
  <<"">>;
encode_peer_cert(PeerCert) ->
  base64:encode(PeerCert).


%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%% Pull in EMQX's OWN definition of the authZ action term. ?AUTHZ_PUBLISH and
%% ?AUTHZ_SUBSCRIBE are the exact constructors emqx_channel uses when it invokes
%% the 'client.authorize' hook (apps/emqx/include/emqx_access_control.hrl). By
%% building test inputs from these macros rather than hand-writing a map, the
%% test is bound to the real EMQX API: if a future EMQX version reshapes the
%% action term, the plugin recompiles against the new macro, these tests feed
%% the new shape into is_pubsub_authorized/3, and the mismatch fails here
%% instead of silently denying all client-device traffic in the field.
-include_lib("emqx/include/emqx_access_control.hrl").

%% These tests pin the contract between EMQX and the GG auth plugin: EMQX 5.1.3
%% passes the authZ action as a map (#{action_type := publish|subscribe, ...}).
%% The original ticket (V2286958564) was a shape mismatch — our clauses expected
%% bare atoms while EMQX passed a map.
%%
%% is_pubsub_authorized/3 routes into is_authorized/… which performs a
%% port-driver IPC call to Client Device Auth. We mock that single seam
%% (gg_port_driver:on_client_check_acl/4) so the tests exercise the map
%% destructuring and dispatch without a running port driver.

is_pubsub_authorized_test_() ->
  {foreach,
    fun() -> meck:new(gg_port_driver, [non_strict, no_link]) end,
    fun(_) -> meck:unload(gg_port_driver) end,
    [
      fun emqx_publish_action_dispatches_to_publish/0,
      fun emqx_subscribe_action_dispatches_to_subscribe/0,
      fun unrecognized_shape_denies_without_consulting_cda/0
    ]}.

%% EMQX's own publish action (?AUTHZ_PUBLISH) must resolve to an mqtt:publish
%% check on the topic. Driving the input from the EMQX macro is what makes this
%% a real API contract test rather than an assertion against our own assumption.
emqx_publish_action_dispatches_to_publish() ->
  meck:expect(gg_port_driver, on_client_check_acl, fun(_, _, _, _) -> {ok, authorized} end),
  ?assertEqual(true, is_pubsub_authorized(?AUTHZ_PUBLISH, <<"cid">>, <<"a/b">>)),
  ?assert(meck:called(gg_port_driver, on_client_check_acl,
    ['_', '_', "mqtt:topic:a/b", "mqtt:publish"])).

%% EMQX's own subscribe action (?AUTHZ_SUBSCRIBE) must resolve to an
%% mqtt:subscribe check on the topic filter.
emqx_subscribe_action_dispatches_to_subscribe() ->
  meck:expect(gg_port_driver, on_client_check_acl, fun(_, _, _, _) -> {ok, authorized} end),
  ?assertEqual(true, is_pubsub_authorized(?AUTHZ_SUBSCRIBE, <<"cid">>, <<"a/b">>)),
  ?assert(meck:called(gg_port_driver, on_client_check_acl,
    ['_', '_', "mqtt:topicfilter:a/b", "mqtt:subscribe"])).

%% Any shape EMQX does not send must fail closed (deny) and must NOT consult
%% CDA. This guards the defensive terminal clause against a future
%% function_clause regression.
unrecognized_shape_denies_without_consulting_cda() ->
  meck:expect(gg_port_driver, on_client_check_acl, fun(_, _, _, _) -> {ok, authorized} end),
  ?assertEqual(false, is_pubsub_authorized(#{unexpected => shape}, <<"cid">>, <<"a/b">>)),
  ?assertEqual(false, is_pubsub_authorized(publish, <<"cid">>, <<"a/b">>)),
  ?assertNot(meck:called(gg_port_driver, on_client_check_acl, ['_', '_', '_', '_'])).

%% pubsub_action_type/1 extracts the action for logging from the EMQX action map
%% or a raw term, and must never throw (logging must not break authZ).
pubsub_action_type_test() ->
  ?assertEqual(publish, pubsub_action_type(?AUTHZ_PUBLISH)),
  ?assertEqual(subscribe, pubsub_action_type(?AUTHZ_SUBSCRIBE)),
  ?assertEqual(some_other_shape, pubsub_action_type(some_other_shape)).

%% An unrecognized result from the C++ port driver must fail closed (deny)
%% rather than raise function_clause. The crash path is what would otherwise
%% put the CDA auth token (a positional argument to is_authorized/6) into a
%% stacktrace and then into the logs, so failing closed here is both the safe
%% authZ outcome and the fix for that exposure.
is_authorized_unrecognized_result_denies_test() ->
  ?assertEqual(?UNAUTHORIZED,
    is_authorized(unexpected_driver_result, _Retries = 0, <<"token">>, <<"cid">>, "mqtt:topic:a/b", "mqtt:publish")).

-endif.
