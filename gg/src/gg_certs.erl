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
  logger:info("Deleted gg cert files: ~p", SoftDeletedFiles).

gg_cert_files() ->
  DataDir = emqx:data_dir(),
  {
    lists:map(fun(File) -> filename:join([DataDir, File]) end, ["key.pem", "cert.pem"]),
    lists:map(fun(File) -> filename:join([DataDir, File]) end, ["deleted.key.pem", "deleted.cert.pem"])
  }.

restore_cert_files() ->
  {Names, SoftDeleteNames} = gg_cert_files(),
  RestoredFiles = rename_files(SoftDeleteNames, Names),
  logger:info("Restored gg cert files: ~p", RestoredFiles).

rename_files(Sources, Dests) ->
  lists:filter(
    fun({Source, Dest}) ->
      case file:rename(Source, Dest) of
        ok -> true;
        _ -> false
      end
    end,
    lists:zip(Sources, Dests)).

%% Request that CDA generate server certificates for EMQX to use.
%% On completion of this function, certificates will be written
%% to the EMQX component work directory, where the EMQX ssl listener
%% is configured to read certs from.
request_certificates() ->
  certificate_request_receiver ! {request_certificates, self()},
  receive
    certs_updated -> ok
  after ?CERT_LOAD_TIMEOUT_MILLIS ->
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
  logger:info("Certificate update request received"),
  register_listener(Caller),
  gg_port_driver:subscribe_to_certificate_updates(fun on_cert_update/0).

register_listener(Pid) ->
  application:set_env(?ENV_APP, on_cert_update, Pid).

trigger_listener() ->
  trigger_listener(application:get_env(?ENV_APP, on_cert_update, undefined)).
trigger_listener(Pid) when is_pid(Pid) ->
  Pid ! certs_updated;
trigger_listener(_) ->
  pass.

on_cert_update() ->
  case catch emqx_mgmt:clean_pem_cache_all() of
    ok -> logger:info("PEM cache cleared");
    Err -> logger:warning("Unable to clear PEM cache: ~p", [Err])
  end,
  trigger_listener().
