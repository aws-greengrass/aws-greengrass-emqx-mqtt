%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_conf).

-export([auth_enabled/0]).

-define(CONF_AUTH_ENABLED, "GREENGRASS_AUTH_ENABLED").
-define(DEFAULT_AUTH_ENABLED, "true").

%% Check if greengrass authN and authZ are enabled.
-spec(auth_enabled() -> boolean()).
auth_enabled() ->
  case os:getenv(?CONF_AUTH_ENABLED, ?DEFAULT_AUTH_ENABLED) of
    "true" -> true;
    _ -> false
  end.
