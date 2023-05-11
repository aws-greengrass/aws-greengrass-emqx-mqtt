%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------
-module(aws_greengrass_emqx_auth_schema).

-include_lib("hocon/include/hoconsc.hrl").
-include_lib("typerefl/include/types.hrl").

-behaviour(hocon_schema).

-export([
  namespace/0,
  roots/0,
  fields/1
]).

namespace() -> "aws_greengrass_emqx_auth".

roots() -> [{"aws_greengrass_emqx_auth", ?HOCON(?R_REF("aws_greengrass_emqx_auth"), #{translate_to => ["aws_greengrass_emqx_auth"]})}].

fields("aws_greengrass_emqx_auth") ->
  [
    {auth_mode,
      ?HOCON(
        hoconsc:enum([enabled, bypass_on_failure, bypass]),
        #{
          default => enabled,
          required => false,
          desc => "Auth mode"
        }
      )},
    {use_greengrass_managed_certificates,
      ?HOCON(
        boolean(),
        #{
          default => true,
          required => false,
          desc => "Use greengrass managed certificates"
        }
      )}
  ].
