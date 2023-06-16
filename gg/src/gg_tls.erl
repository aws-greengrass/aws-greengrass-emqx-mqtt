%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(gg_tls).

-include_lib("emqx/include/emqx.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([enable/1]).

%% Enable CDA verification of client certificates as part of the TLS handshake,
%% by overriding the ssl verification function for the provided emqx listener.
%%
%% NOTE: this will require a restart of the emqx listener.
-spec(enable(atom) -> ok | {error, any()}).
enable(ListenerName) ->
  case gg_listeners:put_verify_fun(ssl, ListenerName, fun custom_verify/3) of
    ok ->
      logger:info("Custom certificate verification enabled on listener ~p:~p", [ssl, ListenerName]);
    {error, Err} -> {error, Err}
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
  case gg_port_driver:verify_client_certificate(CertPem) of
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
