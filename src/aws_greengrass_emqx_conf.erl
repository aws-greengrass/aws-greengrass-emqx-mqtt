%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(aws_greengrass_emqx_conf).

-export([auth_mode/0, use_greengrass_managed_certificates/0]).

-type(auth_mode() :: enabled | bypass_on_failure | bypass).
-type(use_greengrass_managed_certificates() :: true | false).

-export_type([auth_mode/0, use_greengrass_managed_certificates/0]).

-spec(auth_mode() -> auth_mode()).
auth_mode() ->
  application:get_env(aws_greengrass_emqx_auth, auth_mode, enabled).

-spec(use_greengrass_managed_certificates() -> use_greengrass_managed_certificates()).
use_greengrass_managed_certificates() ->
  application:get_env(aws_greengrass_emqx_auth, use_greengrass_managed_certificates, true).
