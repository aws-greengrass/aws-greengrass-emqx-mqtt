%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(tls_custom_certificate_verification).

-include_lib("emqx/include/emqx.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([enable/1]).

%% Enables custom certificate verification
%% by restarting ssl listener with custom certificate verification
-spec(enable(string) -> ok | {error, any()}).
enable(ListenerName) ->
  case aws_greengrass_emqx_listeners:get_listener_config(ssl, ListenerName) of
    listener_not_found -> {error, listener_not_found};
    Conf ->
      NewConf = aws_greengrass_emqx_listeners:set_verify_fun(Conf, fun custom_verify/3),
      restart_listener(ssl, ListenerName, NewConf)
  end.

-spec(restart_listener(atom, string, any()) -> ok | {error, any()}).
restart_listener(Proto, Name, NewConfig) ->
  case aws_greengrass_emqx_listeners:restart_listener(Proto, Name, NewConfig) of
    ok ->
      logger:info("Restarted ~p ~w listener on port ~w with custom certificate verification",
        [Name, Proto, maps:get(listen_on, NewConfig)]),
      ok;
    {error, Reason} ->
      logger:error("Failed to restart ~p ~w listener on port ~w with custom certificate verification: ~p",
        [Name, Proto, maps:get(listen_on, NewConfig), Reason]),
      {error, Reason}
  end.

-spec(custom_verify(OtpCert :: #'OTPCertificate'{}, Event :: {bad_cert, Reason :: atom() |
    {revoked, atom()}} | {extension, #'Extension'{}} | valid | valid_peer | {atom(), atom()},
    InitialUserState :: term()) -> {valid, UserState :: term()} | {fail, Reason :: term()}).
custom_verify(OtpCert, Reason, UserState) ->
  logger:debug("Verifying client certificate for reason: ~p", [Reason]),
  case Reason of
    {extension, _} ->
      {unknown, UserState};
    {bad_cert, unknown_ca} -> verify_client_certificate(OtpCert, UserState);
    {bad_cert, selfsigned_peer} -> verify_client_certificate(OtpCert, UserState);
    valid ->
      logger:debug("Returning valid for valid"),
      {valid, UserState};
    valid_peer ->
      logger:debug("Returning valid for valid_peer"),
      {valid, UserState};
    {_, _} ->
      logger:debug("Client certificate is invalid. Reason: ~p", [Reason]),
      {fail, "invalid certificate"}
  end.

-spec(verify_client_certificate(OtpCert :: #'OTPCertificate'{}, InitialUserState :: term()) ->
  {valid, UserState :: term()} | {fail, Reason :: term()}).
verify_client_certificate(OtpCert, UserState) ->
  CertPem = otpcert_to_pem(OtpCert),
  case port_driver_integration:verify_client_certificate(CertPem) of
    {ok, valid} ->
      logger:debug("Client certificate is valid: ~p", [CertPem]),
      {valid, UserState};
    {ok, invalid} ->
      logger:debug("Client certificate is invalid: ~p", [CertPem]),
      {fail, "invalid certificate"};
    {error, Error} ->
      logger:error("Failed to verify client certificate. Error: ~p", [Error]),
      {fail, Error};
    Unexpected ->
      logger:debug("Unexpected certificate verify result: ~p", [Unexpected]),
      {fail, "invalid certificate"}
  end.

-spec(otpcert_to_pem(OtpCert :: #'OTPCertificate'{}) -> base64:base64_string()).
otpcert_to_pem(OtpCert) ->
  Cert = public_key:pkix_encode('OTPCertificate', OtpCert, otp),
  base64:encode_to_string(Cert).
