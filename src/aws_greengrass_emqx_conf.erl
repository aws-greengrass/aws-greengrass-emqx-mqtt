%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_conf).

-export([auth_enabled/0]).

-define(CONF_AUTH_ENABLED, greengrass_auth_enabled).
-define(DEFAULT_AUTH_ENABLED, true).

%% Check if greengrass authN and authZ are enabled.
-spec(auth_enabled() -> boolean()).
auth_enabled() ->
  case emqx:get_env(?CONF_AUTH_ENABLED) of
    undefined -> ?DEFAULT_AUTH_ENABLED;
    true -> true;
    "true" -> true;
    _ -> false
  end.
