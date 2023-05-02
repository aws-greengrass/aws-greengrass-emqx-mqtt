%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(tls_custom_certificate_verification).

-include_lib("emqx/include/emqx.hrl").
-include_lib("public_key/include/public_key.hrl").

-import(emqx_listeners, [restart_listener/3, parse_listener_id/1]).
-import(port_driver_integration, [verify_client_certificate/1]).

-export([enable/0]).

%% Enables custom certificate verification
%% by restarting ssl listener with custom certificate verification
-spec(enable() -> ok | {error, any()} | nossl).
enable() ->
  case update_ssl_listener_options() of
    nossl -> nossl;
    UpdatedSslListener -> update_ssl_listener_env(UpdatedSslListener)
  end.

-spec(update_ssl_listener_options() -> emqx_listeners:listener() | nossl).
update_ssl_listener_options() ->
  Listeners = emqx_listeners:list(),
  logger:debug("emqx listeners: ~p", [Listeners]),
  case find_ssl_listener(Listeners) of
    nossl -> nossl;
    {ListenerId, ListenerOptions} ->
      {ListenerId, add_custom_verify_to_ssl_options(ListenerOptions)}
  end.

-spec(add_custom_verify_to_ssl_options(Options :: map()) -> map()).
add_custom_verify_to_ssl_options(Options) ->
  case maps:get(ssl_options, Options, undefined) of
    undefined -> Options;
    SslOpts ->
      %% Store verify_fun in the ssl_options, then return the modified options
      maps:put(ssl_options, maps:put(verify_fun, fun custom_verify/3, SslOpts), Options)
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

-spec(find_ssl_listener(Listeners :: list()) -> emqx_listeners:listener() | nossl).
find_ssl_listener([]) -> nossl;
find_ssl_listener([{'ssl:mtls', _} = L | _]) -> L;
find_ssl_listener([_ | Rest]) -> find_ssl_listener(Rest).

-spec(update_ssl_listener_env(emqx_listeners:listener()) -> ok).
update_ssl_listener_env({ListenerId, Conf}) ->
  {ok, #{type := Type, name := Name}} = emqx_listeners:parse_listener_id(ListenerId),
  emqx_listeners:restart_listener(Type, Name, {Conf, Conf}).
