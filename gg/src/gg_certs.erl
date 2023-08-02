%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(gg_certs).

-export([start/0, stop/0]).
-export([request_certificates/0, do_receive_certificate_requests/0]).

-define(ENV_APP, aws_greengrass_emqx_auth). %% TODO rename all to gg

-define(CERT_LOAD_TIMEOUT_MILLIS, 300000). %% 5 minutes

start() ->
  receive_certificate_requests(),
  gg_conf:register_config_change_handler(<<"useGreengrassManagedCertificates">>, fun on_use_greengrass_managed_certificates_change/1).

stop() ->
  certificate_request_receiver ! stop.

on_use_greengrass_managed_certificates_change(_NewValue = true) ->
  restore_cert_files(),
  request_certificates();
on_use_greengrass_managed_certificates_change(_NewValue) ->
  gg_port_driver:unsubscribe_from_certificate_updates(),
  {Names, SoftDeleteNames} = gg_cert_files(),
  SoftDeletedFiles = rename_files(Names, SoftDeleteNames),
  logger:info("Unsubscribed from receiving future gg certificates"),
  logger:info("Deleted gg cert files: ~p", [SoftDeletedFiles]).

gg_cert_files() ->
  DataDir = emqx:data_dir(),
  {
    lists:map(fun(File) -> filename:join([DataDir, File]) end, ["key.pem", "cert.pem"]),
    lists:map(fun(File) -> filename:join([DataDir, File]) end, ["deleted.key.pem", "deleted.cert.pem"])
  }.

restore_cert_files() ->
  {Names, SoftDeleteNames} = gg_cert_files(),
  RestoredFiles = rename_files(SoftDeleteNames, Names),
  logger:info("Restored gg cert files: ~p", [RestoredFiles]).

rename_files(Sources, Dests) ->
  {_, RenamedDests} =
    lists:unzip(
      lists:filter(
        fun({Source, Dest}) ->
          case file:rename(Source, Dest) of
            ok -> true;
            _ -> false
          end
        end,
        lists:zip(Sources, Dests)
      )
    ),
  RenamedDests.

%% Request that CDA generate server certificates for EMQX to use.
%% On completion of this function, certificates will be written
%% to the EMQX component work directory, where the EMQX ssl listener
%% is configured to read certs from.
request_certificates() ->
  certificate_request_receiver ! {request_certificates, self()},
  receive
    certs_updated -> ok;
    already_subscribed -> ok
  after ?CERT_LOAD_TIMEOUT_MILLIS -> %% TODO receive failure rather than waiting
    exit({error, "Timed out waiting for certificate update"})
  end.

receive_certificate_requests() ->
  ListenPID = spawn(?MODULE, do_receive_certificate_requests, []),
  register(certificate_request_receiver, ListenPID).

do_receive_certificate_requests() ->
  receive
    {request_certificates, Caller} ->
      do_request_certificates(Caller),
      do_receive_certificate_requests();
    stop -> ok
  end.

do_request_certificates(Caller) ->
  logger:info("Handling certificate update request"),
  register_receiver(Caller),
  subscribe_to_certificate_updates_once().

subscribe_to_certificate_updates_once() ->
  case application:get_env(?ENV_APP, certificates_requested, false) of
    true -> send_to_receiver(already_subscribed);
    false ->
      gg_port_driver:subscribe_to_certificate_updates(fun on_cert_update/0),
      application:set_env(?ENV_APP, certificates_requested, true)
  end.

register_receiver(Pid) ->
  application:set_env(?ENV_APP, on_cert_update, Pid).

send_to_receiver(Message) ->
  send_to_receiver(Message, application:get_env(?ENV_APP, on_cert_update, undefined)).
send_to_receiver(Message, Pid) when is_pid(Pid) ->
  Pid ! Message;
send_to_receiver(_Message, _) ->
  pass.

on_cert_update() ->
  case catch emqx_mgmt:clean_pem_cache_all() of
    ok -> logger:info("PEM cache cleared");
    Err -> logger:warning("Unable to clear PEM cache: ~p", [Err])
  end,
  send_to_receiver(certs_updated).
