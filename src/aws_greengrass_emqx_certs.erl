%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_certs).

-export([load/0
]).
-import(port_driver_integration, [register_fun/2, request_certificates/0]).

%% Called when the plugin application start
load() ->
  % Register callback, then request certificates. loadAllServerCerts will be called whenever the certificates change.
  port_driver_integration:register_fun(certificate_update, fun cleanPemCache/0),
  port_driver_integration:request_certificates().

% Clean EMQX Pem Cache
cleanPemCache() ->
  try
    ok = emqx_mgmt:clean_pem_cache(),
    logger:info("Finished cleaning pem cache!"),
    ok
  catch
    error:{badmatch, ok} ->
      logger:error("Failed to clean PEM cache!"),
      ok
  end.
