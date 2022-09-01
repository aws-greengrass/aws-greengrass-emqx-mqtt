%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_certs).

-export([load/0]).

load() ->
  case aws_greengrass_emqx_conf:greengrass_broker_server_certificate_mode() of
    enabled ->
      port_driver_integration:register_fun(certificate_update, fun cleanPemCache/0),
      %% Subscribe to certificate updates. On update,
      %% the certificate and its private key are written
      %% to the EMQX component work directory. The EMQX
      %% ssl listener is configured to read these files.
      port_driver_integration:request_certificates();
    disabled ->
      ok
  end.

cleanPemCache() ->
  case catch emqx_mgmt:clean_pem_cache() of
    ok -> logger:info("Finished cleaning pem cache!");
    _ -> logger:error("Failed to clean PEM cache!")
  end.
