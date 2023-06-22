%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(gg_certs).

-export([request_certificates/0]).

-define(CERT_LOAD_TIMEOUT_MILLIS, 300000). %% 5 minutes

-define(REQUEST_CERTS_PROC, gg_request_certificates).
-define(CERTS_UPDATED, certs_updated).

%% Request that CDA generate server certificates for EMQX to use.
%% On completion of this function, certificates will be written
%% to the EMQX component work directory, where the EMQX ssl listener
%% is configured to read certs from.
request_certificates() ->
  request_certificates(gg_conf:use_greengrass_managed_certificates()).
request_certificates(Enabled) when Enabled == true ->
  register(?REQUEST_CERTS_PROC, self()),
  gg_port_driver:register_fun(certificate_update, fun on_cert_update/0),
  gg_port_driver:request_certificates(),
  receive
    ?CERTS_UPDATED -> ok
  after ?CERT_LOAD_TIMEOUT_MILLIS ->
    exit({error, "Timed out waiting for requested server certificates"})
  end;
request_certificates(Enabled) when Enabled == false ->
  ok.

on_cert_update() ->
  case catch emqx_mgmt:clean_pem_cache_all() of
    ok -> logger:info("PEM cache cleared");
    Err -> logger:warning("Unable to clear PEM cache: ~p", [Err])
  end,
  ?REQUEST_CERTS_PROC ! ?CERTS_UPDATED.
