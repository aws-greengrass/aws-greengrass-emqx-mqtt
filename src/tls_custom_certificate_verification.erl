%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(tls_custom_certificate_verification).

-include("emqx.hrl").
-include_lib("public_key/include/public_key.hrl").

-import(port_driver_integration, [verify_client_certificate/1]).

-export([enable/0]).

%% Enables custom certificate verification
%% by restarting ssl listener with custom certificate verification
-spec(enable() -> ok | {error, any()} | nossl).
enable() ->
  case update_ssl_listener_options() of
    nossl -> nossl;
    UpdatedSslListener -> start_updated_ssl_listener(UpdatedSslListener)
  end.

-spec(update_ssl_listener_options() -> emqx_listeners:listener() | nossl).
update_ssl_listener_options() ->
  case find_ssl_listener(emqx:get_env(listeners, [])) of
    nossl -> nossl;
    SslListener ->
      NewListenerOpts = add_custom_verify_to_ssl_options(maps:get('opts', SslListener, [])),
      maps:put('opts', NewListenerOpts, SslListener)
  end.

-spec(add_custom_verify_to_ssl_options(Options :: list()) -> list()).
add_custom_verify_to_ssl_options(Options) ->
  case proplists:get_value(ssl_options, Options) of
    undefined -> Options;
    SslOpts ->
      NewSslOpts = lists:append(SslOpts,
        [{verify_fun,
          {
            fun custom_verify/3, []
          }
        }]),
      replace(Options, ssl_options, NewSslOpts)
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

-spec(start_updated_ssl_listener(emqx_listeners:listener()) -> ok | {error, any()}).
start_updated_ssl_listener(UpdatedSslListener) ->
  case restart_ssl_listener(UpdatedSslListener) of
    ok ->
      logger:info("Restarted ~p ~w listener on port ~w with custom certificate verification",
        [maps:get(name, UpdatedSslListener),
          maps:get(proto, UpdatedSslListener),
          maps:get(listen_on, UpdatedSslListener)]),
      update_ssl_listener_env(UpdatedSslListener),
      ok;
    {error, Reason} ->
      logger:error("Failed to restart ~p ~w listener on port ~w with custom certificate verification: ~p",
        [maps:get(name, UpdatedSslListener),
          maps:get(proto, UpdatedSslListener),
          maps:get(listen_on, UpdatedSslListener),
          Reason]),
      {error, Reason}
  end.

-spec(restart_ssl_listener(emqx_listeners:listener()) -> ok | {error, any()}).
restart_ssl_listener(UpdatedSslListener) ->
  case stop_existing_ssl_listener() of
    ok -> start_listener(UpdatedSslListener);
    {error, Reason} -> {error, Reason}
  end.

-spec(stop_existing_ssl_listener() -> ok | {error, any()}).
stop_existing_ssl_listener() ->
  case find_ssl_listener(emqx:get_env(listeners, [])) of
    nossl -> ok;
    SslListener -> emqx_listeners:stop_listener(SslListener)
  end.

-spec(start_listener(emqx_listeners:listener()) -> ok | {error, any()}).
start_listener(UpdatedSslListener) ->
  case emqx_listeners:start_listener(UpdatedSslListener) of
    ok -> ok;
    Error -> {error, Error}
  end.

-spec(find_ssl_listener(Listeners :: list()) -> emqx_listeners:listener() | nossl).
find_ssl_listener([]) -> nossl;
find_ssl_listener([#{name := "external", proto := 'ssl'} = L | _]) -> L;
find_ssl_listener([_ | Rest]) -> find_ssl_listener(Rest).

-spec(update_ssl_listener_env(emqx_listeners:listener()) -> ok).
update_ssl_listener_env(SslListener) ->
  emqx_listeners:update_listeners_env('update', SslListener).

replace(Opts, Key, Value) -> [{Key, Value} | proplists:delete(Key, Opts)].
