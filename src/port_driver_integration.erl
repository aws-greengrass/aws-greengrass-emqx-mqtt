%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(port_driver_integration).

-export([start/0, stop/0, init/1]).
-export([ on_client_authenticate/2
        , on_client_connect/2
        , on_client_connected/2
        , on_client_disconnected/2
        , on_client_check_acl/4
        ]).

-include("emqx.hrl").
-import(os,[getenv/1]).

%% OPERATIONS
-define(ON_CLIENT_CONNECT, 0).
-define(ON_CLIENT_CONNECTED, 1).
-define(ON_CLIENT_DISCONNECT, 2).
-define(ON_CLIENT_AUTHENTICATE, 3).
-define(ON_CLIENT_CHECK_ACL, 4).

%% RETURN CODES
-define(RETURN_CODE_SUCCESS, 0).

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

on_client_connect(ClientId, CertPem) ->
    call_port({on_client_connect, ClientId, CertPem}).

on_client_connected(ClientId, CertPem) ->
    call_port({on_client_connected, ClientId, CertPem}).

on_client_disconnected(ClientId, CertPem) ->
    call_port({on_client_disconnected, ClientId, CertPem}).

on_client_authenticate(ClientId, CertPem) ->
    call_port({on_client_authenticate, ClientId, CertPem}).

on_client_check_acl(ClientId, CertPem, Topic, PubSub) ->
    call_port({on_client_check_acl, ClientId, CertPem, Topic, PubSub}).

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

encode({on_client_connect, ClientId, CertPem}) ->
    encode({ClientId, CertPem, ?ON_CLIENT_CONNECT});
encode({on_client_connected, ClientId, CertPem}) ->
    encode({ClientId, CertPem, ?ON_CLIENT_CONNECTED});
encode({on_client_disconnected, ClientId, CertPem}) ->
    encode({ClientId, CertPem, ?ON_CLIENT_DISCONNECT});
encode({on_client_authenticate, ClientId, CertPem}) ->
    encode({ClientId, CertPem, ?ON_CLIENT_AUTHENTICATE});
encode({on_client_check_acl, ClientId, CertPem, Topic, PubSub}) ->
    {ClientIdBinary, CertPemBinary} = convert_to_binary({ClientId, CertPem}),
    TopicBinary = convert_to_binary({Topic}),
    PubSubBinary = acl_action_to_binary(PubSub),
    [<<?ON_CLIENT_CHECK_ACL, ClientIdBinary/binary, CertPemBinary/binary,
        TopicBinary/binary, PubSubBinary/binary>>];
encode({A, B, Operation}) ->
    {ABinary, BBinary} = convert_to_binary({A, B}),
    [<<Operation, ABinary/binary, BBinary/binary>>].

decode(Data) -> Data.

convert_to_binary({A}) ->
    %% TODO: Improve this - redundant conversion?
    term_to_binary(binary_to_list(A));
convert_to_binary({A, B}) ->
    {convert_to_binary({A}), convert_to_binary({B})}.

acl_action_to_binary(PubSub) ->
    %% TODO: Improve this - redundant conversion?
    term_to_binary(atom_to_list(PubSub)).
