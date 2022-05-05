%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_certs).

-export([load/0
]).

-define(GG_CERT_REL_DIR, "etc/greengrass_certs/").
-define(CA_FILE, ?GG_CERT_REL_DIR ++ "greengrass_ca.pem").
-define(KEY_FILE, ?GG_CERT_REL_DIR ++ "greengrass_emqx.key").
-define(PEM_FILE, ?GG_CERT_REL_DIR ++ "greengrass_emqx.pem").

%% Called when the plugin application start
load() ->
  loadAllServerCerts().

%% Check for greengrass_ca.pem, greengrass_emqx.key, greengrass_emqx.pem, and reload the PEM cache
loadAllServerCerts() ->
  checkServerCert(?CA_FILE),
  checkServerCert(?KEY_FILE),
  checkServerCert(?PEM_FILE),
  logger:info("Validated server certs in ~p", [?GG_CERT_REL_DIR]),
  cleanPemCache().

% Write server cert Data to specified location FileName
checkServerCert(FileName) ->
  FileExists = filelib:is_regular(FileName),
  if
    FileExists ->
      %Until we have proper cert rotation in place, just exit
      logger:error("Cert file ~p already exists!", [FileName]);
%%      exit("Found existing cert file during startup!: ", [FileName]);
    true ->
      logger:warn("File does not exist yet!...", [FileName])
  end.

% Clean EMQX Pem Cache
cleanPemCache() ->
  try
    ok = emqx_mgmt:clean_pem_cache()
  catch
    error:{badmatch, ok} -> logger:error("Failed to clean PEM cache!"),
    exit("Failed to clean PEM cache!")
  end,
  logger:info("Finished cleaning pem cache!").
