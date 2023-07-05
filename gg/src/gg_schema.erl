%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------
-module(gg_schema).

-include_lib("hocon/include/hoconsc.hrl").
-include_lib("typerefl/include/types.hrl").

-behaviour(hocon_schema).

-export([
  namespace/0,
  roots/0,
  fields/1
]).

namespace() -> "gg".

roots() -> ["gg"].

fields("gg") ->
  [
    {authMode,
      ?HOCON(
        hoconsc:enum([enabled, bypass_on_failure, bypass]),
        #{
          default => enabled,
          required => false,
          desc => "Auth mode"
        }
      )},
    {useGreengrassManagedCertificates,
      ?HOCON(
        boolean(),
        #{
          default => true,
          required => false,
          desc => "Use greengrass managed certificates"
        }
      )}
  ].
