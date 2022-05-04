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
        , verify_client_certificate/1
        ]).

-include("emqx.hrl").
-import(os,[getenv/1]).

%% OPERATIONS
-define(ON_CLIENT_CONNECT, 0).
-define(ON_CLIENT_CONNECTED, 1).
-define(ON_CLIENT_DISCONNECT, 2).
-define(ON_CLIENT_AUTHENTICATE, 3).
-define(ON_CLIENT_CHECK_ACL, 4).
-define(VERIFY_CLIENT_CERTIFICATE, 5).

%% OPERATION RESULTS
-define(OPERATION_RESULT_FAIL, 0).
-define(OPERATION_RESULT_PASS, 1).
-define(OPERATION_RESULT_UNKNOWN, 2).

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
    {ReturnCode, ResultBinary} = call_port({on_client_connect, ClientId, CertPem}),
    case {ReturnCode, operation_result(ResultBinary)} of
        {ok, ?OPERATION_RESULT_PASS} -> pass;
        {ok, ?OPERATION_RESULT_FAIL} -> fail;
        {ok, ?OPERATION_RESULT_UNKNOWN} -> fail;
        {error, Error} -> Error
    end.

on_client_connected(ClientId, CertPem) ->
    {ReturnCode, ResultBinary} = call_port({on_client_connected, ClientId, CertPem}),
    case {ReturnCode, operation_result(ResultBinary)} of
        {ok, ?OPERATION_RESULT_PASS} -> pass;
        {ok, ?OPERATION_RESULT_FAIL} -> fail;
        {ok, ?OPERATION_RESULT_UNKNOWN} -> fail;
        {error, Error} -> Error
    end.

on_client_disconnected(ClientId, CertPem) ->
    {ReturnCode, ResultBinary} = call_port({on_client_disconnected, ClientId, CertPem}),
    case {ReturnCode, operation_result(ResultBinary)} of
        {ok, ?OPERATION_RESULT_PASS} -> pass;
        {ok, ?OPERATION_RESULT_FAIL} -> fail;
        {ok, ?OPERATION_RESULT_UNKNOWN} -> fail;
        {error, Error} -> Error
    end.

on_client_authenticate(ClientId, CertPem) ->
    {ReturnCode, ResultBinary} = call_port({on_client_authenticate, ClientId, CertPem}),
    case {ReturnCode, operation_result(ResultBinary)} of
        {ok, ?OPERATION_RESULT_PASS} -> {ok, valid_client};
        {ok, ?OPERATION_RESULT_FAIL} -> {ok, invalid_client};
        {ok, ?OPERATION_RESULT_UNKNOWN} -> {ok, invalid_client};
        {error, Error} -> {error, Error}
    end.

on_client_check_acl(ClientId, CertPem, Topic, PubSub) ->
    {ReturnCode, ResultBinary} = call_port({on_client_check_acl, ClientId, CertPem, Topic, PubSub}),
    case {ReturnCode, operation_result(ResultBinary)} of
        {ok, ?OPERATION_RESULT_PASS} -> {ok, authorized};
        {ok, ?OPERATION_RESULT_FAIL} -> {ok, unauthorized};
        {ok, ?OPERATION_RESULT_UNKNOWN} -> {ok, unauthorized};
        {error, Error} -> {error, Error}
    end.

verify_client_certificate(CertPem) ->
    {ReturnCode, ResultBinary} = call_port({verify_client_certificate, CertPem}),
    case {ReturnCode, operation_result(ResultBinary)} of
        {ok, ?OPERATION_RESULT_PASS} -> {ok, valid_cert};
        {ok, ?OPERATION_RESULT_FAIL} -> {ok, invalid_cert};
        {ok, ?OPERATION_RESULT_UNKNOWN} -> {ok, invalid_cert};
        {error, Error} -> {error, Error}
    end.

call_port(Msg) ->
    process ! {call, self(), Msg},
    receive
	{process, Data} ->
	    logger:debug("Data received from port: ~p ~n", [Data]),
	    [ReturnCode|Result] = Data,
	    case ReturnCode of
	        ?RETURN_CODE_SUCCESS -> {ok, Result};
	        ErrorReturnCode ->
	            logger:error("Error from driver. Code: ~p ~n", [ErrorReturnCode]),
	            {error, error_from_driver}
	    end
    end.

operation_result(Result) when is_binary(Result) ->
    % assuming the size of result-type returned by port-driver is platform dependent 
    % check the first byte of binary result, ignoring rest of the zero-bytes
    case binary:at(Result, 0) of
	?OPERATION_RESULT_PASS -> ?OPERATION_RESULT_PASS;
	?OPERATION_RESULT_FAIL -> ?OPERATION_RESULT_FAIL;
	?OPERATION_RESULT_UNKNOWN -> ?OPERATION_RESULT_UNKNOWN
    end;
operation_result(Result) -> Result.
	

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
encode({verify_client_certificate, CertPem}) ->
    CertPemBinary = convert_to_binary({CertPem}),
    [<<?VERIFY_CLIENT_CERTIFICATE, CertPemBinary/binary>>];
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

