%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(auth_token_store).

-export([init/0, save_token/2, delete_token/1, lookup_token/1, close/0]).

-define(AUTH_TOKEN_TABLE, client_device_auth_token_store).

init() ->
  logger:debug("Initializing auth token store"),
  ets:new(?AUTH_TOKEN_TABLE, [named_table, protected, set, {keypos, 1}]).

save_token(CertPem, AuthToken) ->
  %% maps sha256 hash of cert to its AuthToken
  %% to avoid storing cert itself
  ets:insert(?AUTH_TOKEN_TABLE, {hash_cert(CertPem), AuthToken}).

delete_token(CertPem) ->
  ets:delete(?AUTH_TOKEN_TABLE, hash_cert(CertPem)).

lookup_token(CertPem) ->
  ets:lookup(?AUTH_TOKEN_TABLE, hash_cert(CertPem)).

close() ->
  ets:delete(?AUTH_TOKEN_TABLE).

%% Creates hex-string for sha256 hash 
hash_cert(CertPem) ->
  io_lib:format("~64.16.0b", 
    [binary:decode_unsigned(crypto:hash(sha256, CertPem))]).
