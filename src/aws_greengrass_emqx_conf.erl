%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_conf).

-export([greengrass_authorization_mode/0, greengrass_broker_server_certificate_mode/0]).

-type(greengrass_authorization_mode() :: enabled | enabled_bypass_on_failure | bypass).
-type(greengrass_broker_server_certificate_mode() :: enabled | disabled).

-export_type([greengrass_authorization_mode/0, greengrass_broker_server_certificate_mode/0]).

-spec(greengrass_authorization_mode() -> greengrass_authorization_mode()).
greengrass_authorization_mode() ->
  application:get_env(aws_greengrass_emqx_auth, greengrass_authorization_mode, enabled).

-spec(greengrass_broker_server_certificate_mode() -> greengrass_broker_server_certificate_mode()).
greengrass_broker_server_certificate_mode() ->
  application:get_env(aws_greengrass_emqx_auth, greengrass_broker_server_certificate_mode, enabled).
