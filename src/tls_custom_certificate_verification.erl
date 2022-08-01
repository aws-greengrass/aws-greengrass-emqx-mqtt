%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(tls_custom_certificate_verification).

-include("emqx.hrl").
-include_lib("public_key/include/public_key.hrl").

-import(port_driver_integration, [verify_client_certificate/1]).
-import(aws_greengrass_emqx_auth_listeners, [get_ssl_listener/0, set_custom_verify_fun/2, remove_custom_verify_fun/2, restart_listener/1]).

-export([enable/0, disable/0]).

%% Enables custom certificate verification
%% by restarting ssl listener with custom certificate verification
-spec(enable() -> ok | {error, any()} | nossl).
enable() ->
  case get_ssl_listener() of
    false ->
      nossl;
    Listener ->
      UpdatedListener = set_custom_verify_fun(Listener, fun custom_verify/3),
      restart_updated_listener(UpdatedListener)
  end.

%% Disables custom certificate verification
%% by restarting ssl listener without custom certificate verification
-spec(disable() -> ok | {error, any()} | nossl).
disable() ->
  case get_ssl_listener() of
    false ->
      nossl;
    Listener ->
      UpdatedListener = remove_custom_verify_fun(Listener, fun custom_verify/3),
      restart_updated_listener(UpdatedListener)
  end.

%% Restart the provided listener and log the result
-spec(restart_updated_listener(emqx_listeners:listener()) -> ok).
restart_updated_listener(Listener) ->
  Name = maps:get(name, Listener),
  Proto = maps:get(proto, Listener),
  ListenOn = maps:get(listen_on, Listener),
  case restart_listener(Listener) of
    ok ->
      logger:info("Restarted ~p ~w listener on port ~w with custom certificate verification",
        [Name, Proto, ListenOn]);
    {error, Reason} ->
      logger:error("Failed to restart ~p ~w listener on port ~w with custom certificate verification: ~p",
        [Name, Proto, ListenOn, Reason])
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
