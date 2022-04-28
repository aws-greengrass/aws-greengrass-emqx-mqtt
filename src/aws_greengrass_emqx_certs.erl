%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_certs).

-export([ load/1
]).

-define(GG_CERT_REL_DIR, "etc/greengrass_certs/").
-define(CA_FILE, ?GG_CERT_REL_DIR ++ "greengrass_ca.pem").
-define(KEY_FILE, ?GG_CERT_REL_DIR ++ "greengrass_emqx.key").
-define(PEM_FILE, ?GG_CERT_REL_DIR ++ "greengrass_emqx.pem").

%% Called when the plugin application start
load(Env) ->
  loadAllServerCerts().

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
% Retrieves hardcoded certs from the following locations and rewrites them to the same dir without a .bak extension
% etc/emqx.conf must be configured to accept the files with mTLS
% 'etc/greengrass_certs/greengrass_ca.pem.bak'
% 'etc/greengrass_certs/greengrass_emqx.pem.bak'
% 'etc/greengrass_certs/greengrass_emqx.key.bak'
retrieveServerCert(FileName) ->
  BakFileName = FileName ++ ".bak",
  logger:info("Retrieving cert from file ~p...~n", [BakFileName]),

  FileExists = filelib:is_regular(BakFileName),
  if
    FileExists ->
      logger:info("Found cert file ~p", [BakFileName]);
    true ->
      logger:error("Cert file ~p does not exist!", [BakFileName]),
      exit("Cert file does not exist at startup: ", [BakFileName])
  end,

  {ok, Data} = file:read_file(BakFileName),
  binary:split(Data, [<<"\n">>], [global]),
  logger:info("Read in cert file ~p", [BakFileName]),

  Blank = string:is_empty(string:trim(Data)),
  if
    Blank ->
      logger:error("Read cert file ~p is empty!", [BakFileName]),
      exit("Found empty cert file during startup: ", [BakFileName]);
    true ->
      logger:info("Validated cert file is not empty")
  end,
  Data.

% Write server cert Data to specified location FileName
writeServerCert(FileName, Data) ->
  FileExists = filelib:is_regular(FileName),
  if
    FileExists ->
      %Until we have proper cert rotation in place, just exit
      logger:error("Cert file ~p already exists!", [FileName]),
      exit("Found existing cert file during startup!: ", [FileName]);
    true ->
      logger:info("Writing cert to file ~p...", [FileName])
  end,
  file:write_file(FileName, Data),
  logger:info("Wrote to file ~p", [FileName]).

% Clean EMQX Pem Cache via the EMQX CLI
cleanPemCache() ->
  Cmd = "bin/emqx_ctl pem_cache clean all",
  CacheStdout = string:trim(os:cmd(Cmd)),
  if
    CacheStdout == "PEM cache clean OK" ->
      logger:info("Succesfully cleaned PEM cache");
    true ->
      logger:error("Error cleaning PEM cache"),
      exit("Error cleaning PEM cache!")
  end,
  logger:info("Finished cleaning pem cache with output ~p and status ~p", [CacheStdout, ok]).
