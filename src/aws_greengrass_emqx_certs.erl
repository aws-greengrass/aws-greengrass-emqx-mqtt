%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_certs).

-export([ load/0
]).

-define(GG_CERT_REL_DIR, "etc/greengrass_certs/").
-define(CA_FILE, ?GG_CERT_REL_DIR ++ "greengrass_ca.pem").
-define(KEY_FILE, ?GG_CERT_REL_DIR ++ "greengrass_emqx.key").
-define(PEM_FILE, ?GG_CERT_REL_DIR ++ "greengrass_emqx.pem").

%% Called when the plugin application start
load() ->
  loadAllServerCerts().

%% Load in greengrass_ca.pem, greengrass_emqx.key, greengrass_emqx.pem, and reload the PEM cache
loadAllServerCerts() ->
  CaCert = retrieveServerCert(?CA_FILE),
  writeServerCert(?CA_FILE, CaCert),

  Key = retrieveServerCert(?KEY_FILE),
  writeServerCert(?KEY_FILE, Key),

  Pem = retrieveServerCert(?PEM_FILE),
  writeServerCert(?PEM_FILE, Pem),

  logger:info("Successfully wrote all server certs to ~p", [?GG_CERT_REL_DIR]),
  cleanPemCache().

% Temporary function that needs to be replaced with CDA retrieval
% etc/emqx.conf must be configured to accept the files with mTLS
% Retrieves hardcoded certs from the following locations and rewrites them to the same dir without a .bak extension
% 'etc/greengrass_certs/greengrass_ca.pem.bak'
% 'etc/greengrass_certs/greengrass_emqx.pem.bak'
% 'etc/greengrass_certs/greengrass_emqx.key.bak'
retrieveServerCert(FileName) ->
  BakFileName = FileName ++ ".bak",
  logger:debug("Retrieving cert from file ~p...", [BakFileName]),

  FileExists = filelib:is_regular(BakFileName),
  if
    FileExists ->
      logger:debug("Found cert file ~p", [BakFileName]);
    true ->
      logger:error("Cert file ~p does not exist!", [BakFileName]),
      exit("Cert file does not exist at startup: ", [BakFileName])
  end,

  RetrievedData = case file:read_file(BakFileName) of
    {ok, Data} -> Data;
    {error, Reason} ->
      {"Error reading file!", error, Reason},
      exit("Failed to read file: ", [BakFileName])
  end,
  logger:debug("Read in cert file ~p", [BakFileName]),

  IsBlank = string:is_empty(string:trim(RetrievedData)),
  if
    IsBlank ->
      logger:error("Read cert file ~p is empty!", [BakFileName]),
      exit("Found empty cert file during startup: ", [BakFileName]);
    true ->
      logger:debug("Validated cert file is not empty")
  end,
  RetrievedData.

% Write server cert Data to specified location FileName
writeServerCert(FileName, Data) ->
  FileExists = filelib:is_regular(FileName),
  if
    FileExists ->
      %Until we have proper cert rotation in place, just exit
      logger:error("Cert file ~p already exists!", [FileName]),
      exit("Found existing cert file during startup!: ", [FileName]);
    true ->
      logger:debug("Writing cert to file ~p...", [FileName])
  end,

  case file:write_file(FileName, Data) of
    ok -> ok;
    {error, Reason} ->
      {"Error writing to file ~p!", [FileName], error, Reason},
      exit("Failed to write to file: ", [FileName])
  end,
  logger:debug("Wrote to file ~p", [FileName]).

% Clean EMQX Pem Cache
cleanPemCache() ->
  try
    ok = emqx_mgmt:clean_pem_cache()
  catch
    error:{badmatch, ok} -> logger:error("Failed to clean PEM cache!"),
    exit("Failed to clean PEM cache!")
  end,
  logger:info("Finished cleaning pem cache!").
