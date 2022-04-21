%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(port_driver_integration).

-export([start/0, stop/0, init/1]).
-export([on_client_authenticate/1]).

-include("emqx.hrl").
-import(os,[getenv/1]).

%% OPERATIONS
-define(ON_CLIENT_AUTHENTICATE, 1).

%% RETURN CODES
-define(RETURN_CODE_SUCCESS, 0).

start() ->
    PortDriverDir = getenv("PORT_DRIVER_DIR"),
    PortDriverName = getenv("PORT_DRIVER_NAME"),
    logger:debug("Loading port driver ~p from dir ~p", [PortDriverName, PortDriverDir]),

    case erl_ddll:load_driver(PortDriverDir, PortDriverName) of
        ok -> ok;
        {error, already_loaded} -> ok;
        _ -> exit({error, could_not_load_driver})
    end,
    logger:debug("Loaded port_driver"),
    spawn(?MODULE, init, [PortDriverName]).

stop() ->
    logger:info("Stopping port driver integration"),
    process ! stop.

init(PortDriver) ->
    register(process, self()),
    Port = open_port({spawn, PortDriver}, [binary]),
    loop(Port).

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, encode(Msg)}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {process, decode(Data)}
	    end,
	    loop(Port);
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

on_client_authenticate(ClientId) ->
    call_port({on_client_authenticate, ClientId}).

call_port(Msg) ->
    process ! {call, self(), Msg},
    receive
	{process, Data} ->
	    logger:error("Data received from port: ~p ~n", [Data]),
	    [ReturnCode|Result] = Data,
	    case ReturnCode of
	        ?RETURN_CODE_SUCCESS -> {ok, Result};
	        ErrorReturnCode ->
	            logger:error("Error from driver. Code: ~p ~n", [ErrorReturnCode]),
	            {error, error_from_driver}
	    end
    end.

encode({on_client_authenticate, ClientId}) ->
%% TODO: Improve this - redundant conversion?
    ClientId_binary = term_to_binary(binary_to_list(ClientId)),
    Enc = [<<?ON_CLIENT_AUTHENTICATE, ClientId_binary/binary>>],
    logger:debug("Encoded ~p to: ~p~n",[ClientId, Enc]),
    Enc.

decode(String) -> String.