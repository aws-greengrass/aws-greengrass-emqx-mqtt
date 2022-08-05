%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_conf).

-import(port_driver_integration, [get_config/1, register_config_change_handler/2]).

-export([auth_enabled/0, when_auth_enabled_changes/1]).

-define(CONF_AUTH_ENABLED, "authEnabled").
-define(DEFAULT_USE_CDA_AUTH, true).

%% Check if greengrass authN and authZ are enabled via greengrass configuration
-spec(auth_enabled() -> boolean()).
auth_enabled() ->
  case port_driver_integration:get_config(?CONF_AUTH_ENABLED) of
    undefined -> ?DEFAULT_USE_CDA_AUTH;
    Val when is_boolean(Val) -> Val;
    _ -> throw({error, io:format("Invalid value for configuration property ~p", [?CONF_AUTH_ENABLED])})
  end.

when_auth_enabled_changes(Fun) ->
  register_config_change_handler(?CONF_AUTH_ENABLED, Fun).
