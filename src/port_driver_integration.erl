%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(port_driver_integration).

-export([start/0, stop/0, init/1]).
-export([on_client_authenticate/2
  , on_client_connect/2
  , on_client_connected/2
  , on_client_disconnected/2
  , on_client_check_acl/4
]).

-include("emqx.hrl").

%% OPERATIONS
-define(ON_CLIENT_CONNECT, 0).
-define(ON_CLIENT_CONNECTED, 1).
-define(ON_CLIENT_DISCONNECT, 2).
-define(ON_CLIENT_AUTHENTICATE, 3).
-define(ON_CLIENT_CHECK_ACL, 4).

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
  spawn(?MODULE, init, [Port]).

stop() ->
  logger:info("Stopping port driver integration"),
  process ! stop.

init(PortDriver) ->
  register(process, self()),
  Port = open_port({spawn, PortDriver}, [binary]),
  loop(Port).

loop(Port) ->
  loop(Port, maps:new(), counters:new(1, [atomics])).

loop(Port, Inflight, Counter) ->
  receive
    {call, Caller, Msg, async} ->
      RequestId = counters:get(Counter, 1),
      counters:add(Counter, 1, 1),
      Port ! {self(), {command, term_to_binary([Msg, RequestId])}},
      receive
        {Port, {data, Data}} ->
          Caller ! {process, Data},
          NewInflight = maps:put(RequestId, Caller, Inflight),
          loop(Port, NewInflight, Counter)
      end;
    {Port, Id, {data, Data}} ->
      Caller = maps:get(Id, Inflight),
      NewInflight = maps:remove(Id, Inflight),
      Caller ! {process, Data},
      loop(Port, NewInflight, Counter);
    {call, Caller, Msg} ->
      Port ! {self(), {command, term_to_binary([Msg])}},
      receive
        {Port, {data, Data}} ->
          Caller ! {process, Data}
      end,
      loop(Port, Inflight, Counter);
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

on_client_connect(ClientId, CertPem) ->
  call_port({?ON_CLIENT_CONNECT, ClientId, CertPem}, async).

on_client_connected(ClientId, CertPem) ->
  call_port({?ON_CLIENT_CONNECTED, ClientId, CertPem}).

on_client_disconnected(ClientId, CertPem) ->
  call_port({?ON_CLIENT_DISCONNECT, ClientId, CertPem}).

on_client_authenticate(ClientId, CertPem) ->
  call_port({?ON_CLIENT_AUTHENTICATE, ClientId, CertPem}).

on_client_check_acl(ClientId, CertPem, Topic, PubSub) ->
  call_port({?ON_CLIENT_CHECK_ACL, ClientId, CertPem, Topic, PubSub}).

receive_back() ->
  receive
    {process, Data} ->
      logger:error("Data received from port: ~p ~n", [Data]),
      [ReturnCode | Result] = Data,
      case ReturnCode of
        ?RETURN_CODE_SUCCESS -> {ok, Result};
        ?RETURN_CODE_ASYNC ->
          logger:error("Got async, sync call, waiting now"),
          receive
            {process, NewData} ->
              logger:error("Data received from port: ~p ~n", [NewData]),
              [ReturnCode2 | Result2] = NewData,
              case ReturnCode2 of
                ?RETURN_CODE_SUCCESS -> {ok, Result2};
                ErrorReturnCode ->
                  logger:error("Error from driver. Code: ~p ~n", [ErrorReturnCode]),
                  {error, error_from_driver}
              end
          end;
        ErrorReturnCode ->
          logger:error("Error from driver. Code: ~p ~n", [ErrorReturnCode]),
          {error, error_from_driver}
      end
  end.

call_port(Msg, async) ->
  process ! {call, self(), Msg, async},
  receive_back().

call_port(Msg) ->
  process ! {call, self(), Msg},
  receive_back().
