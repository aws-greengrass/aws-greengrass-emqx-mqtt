%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(port_driver_integration).

-export([start/0, stop/0, init/2]).
-export([get_auth_token/2
  , on_client_check_acl/4
  , verify_client_certificate/1
  , register_fun/2
  , request_certificates/0
]).

-include("emqx.hrl").

%% OPERATIONS
-define(GET_CLIENT_DEVICE_AUTH_TOKEN, 3).
-define(ON_CLIENT_CHECK_ACL, 4).
-define(VERIFY_CLIENT_CERTIFICATE, 5).
-define(SUBSCRIBE_TO_CERTIFICATE_UPDATES, 6).

%% RETURN CODES
-define(RETURN_CODE_SUCCESS, 0).
-define(RETURN_CODE_ASYNC, 100).

start() ->
  Dir = case code:priv_dir(aws_greengrass_emqx_auth) of
          {error, bad_name} ->
            case code:which(aws_greengrass_emqx_auth) of
              Filename when is_list(Filename) ->
                filename:join(
                  [filename:dirname(Filename), "../priv"]);
              _ ->
                "../priv"
            end;
          Priv -> Priv
        end,
  Port = "port_driver",
  logger:info("Loading port driver ~p from dir ~p", [Port, Dir]),

  case erl_ddll:load_driver(Dir, Port) of
    ok -> ok;
    {error, already_loaded} -> ok;
    _ -> exit({error, could_not_load_driver})
  end,
  logger:debug("Loaded port_driver"),

  spawn(?MODULE, init, [Port, self()]),
  receive
    %% ensure process is registered so we can
    %% safely send messages without race conditions
    port_driver_initialized -> ok
  end.

stop() ->
  logger:info("Stopping port driver integration"),
  greengrass_port_driver ! stop.

init(PortDriver, CallerPID) ->
  register(greengrass_port_driver, self()),
  CallerPID ! port_driver_initialized,
  Port = open_port({spawn, PortDriver}, [binary]),
  loop(Port).

empty() -> ok.

loop(Port) ->
  loop(Port, maps:new(), counters:new(1, [atomics]), maps:new()).

loop(Port, Inflight, Counter, FunMap) ->
  receive
    % register callback
    {register_fun, Atom, Fun} ->
      loop(Port, Inflight, Counter, maps:put(Atom, Fun, FunMap));
    % handle event type from C++ by finding a registered callback (if any)
    {Port, event, EventType} ->
      Fun = maps:get(EventType, FunMap, fun empty/0),
      Fun(),
      loop(Port, Inflight, Counter, FunMap);
    {call, Caller, Msg, async} ->
      RequestId = counters:get(Counter, 1),
      counters:add(Counter, 1, 1),
      Port ! {self(), {command, term_to_binary([Msg, RequestId])}},
      receive
        {Port, {data, Data}} ->
          [ReturnCode | _] = Data,
          case ReturnCode of
            ?RETURN_CODE_ASYNC ->
              NewInflight = maps:put(RequestId, Caller, Inflight),
              loop(Port, NewInflight, Counter, FunMap);
            _OtherReturnCode ->
              Caller ! {greengrass_port_driver, Data},
              loop(Port, Inflight, Counter, FunMap)
          end
      end;
    {Port, Id, {data, Data}} ->
      Caller = maps:get(Id, Inflight),
      NewInflight = maps:remove(Id, Inflight),
      Caller ! {greengrass_port_driver, Data},
      loop(Port, NewInflight, Counter, FunMap);
    {call, Caller, Msg} ->
      Port ! {self(), {command, term_to_binary([Msg])}},
      receive
        {Port, {data, Data}} ->
          Caller ! {greengrass_port_driver, Data}
      end,
      loop(Port, Inflight, Counter, FunMap);
    stop ->
      Port ! {self(), close},
      receive
        {Port, closed} ->
          exit(normal)
      end;
    {'EXIT', Port, Reason} ->
      logger:error("Port terminated ~p", [Reason]),
      exit(port_terminated)
  end.

get_auth_token(ClientId, CertPem) ->
  call_port({?GET_CLIENT_DEVICE_AUTH_TOKEN, ClientId, CertPem}, async).

on_client_check_acl(ClientId, AuthToken, Topic, PubSub) ->
  call_port({?ON_CLIENT_CHECK_ACL, ClientId, AuthToken, Topic, PubSub}, async).

verify_client_certificate(CertPem) ->
  call_port({?VERIFY_CLIENT_CERTIFICATE, CertPem}, async).

request_certificates() ->
  call_port({?SUBSCRIBE_TO_CERTIFICATE_UPDATES}).

register_fun(Atom, Fun) ->
  greengrass_port_driver ! {register_fun, Atom, Fun}.

receive_back() ->
  receive
    {greengrass_port_driver, Data} ->
      logger:debug("Data received from port: ~p ~n", [Data]),
      [ReturnCode | Result] = Data,
      case ReturnCode of
        ?RETURN_CODE_SUCCESS -> {ok, Result};
        ErrorReturnCode ->
          logger:error("Error from driver. Code: ~p ~n", [ErrorReturnCode]),
          {error, error_from_driver}
      end
  end.

call_port(Msg, async) ->
  greengrass_port_driver ! {call, self(), Msg, async},
  receive_back().

call_port(Msg) ->
  greengrass_port_driver ! {call, self(), Msg},
  receive_back().
