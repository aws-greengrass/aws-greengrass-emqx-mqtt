%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_certs).

-export([load/0]).
-import(port_driver_integration, [register_fun/2, request_certificates/0]).
-import(aws_greengrass_emqx_auth_app, [plugin_status/0]).

%% Called when the plugin application start
load() ->
  % Register callback, then request certificates. loadAllServerCerts will be called whenever the certificates change.
  port_driver_integration:register_fun(certificate_update, fun cleanPemCache/0),
  port_driver_integration:request_certificates().

% Clean EMQX Pem Cache
-spec(cleanPemCache() -> ok).
cleanPemCache() ->
  case plugin_status() of
    active ->
      case catch emqx_mgmt:clean_pem_cache() of
        ok -> logger:info("Finished cleaning pem cache!");
        _ -> logger:error("Failed to clean PEM cache!")
      end;
    inactive ->
      ok
  end.
