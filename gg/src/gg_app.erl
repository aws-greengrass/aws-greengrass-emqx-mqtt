%%--------------------------------------------------------------------
%%  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
%%  SPDX-License-Identifier: Apache-2.0
%%--------------------------------------------------------------------

-module(gg_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  {ok, Sup} = gg_sup:start_link(),
  gg_port_driver:start(),
  gg_certs:start(),
  gg_conf:register_config_change_handler(<<"useGreengrassManagedCertificates">>, fun on_use_greengrass_managed_certificates_change/1),
  gg_conf:start(),
  gg:load(application:get_all_env()),
  {ok, Sup}.

stop(_State) ->
  gg_certs:stop(),
  gg_conf:stop(),
  gg:unload(),
  gg_port_driver:stop().

on_use_greengrass_managed_certificates_change(_NewValue = true) ->
  gg_certs:request_certificates();
on_use_greengrass_managed_certificates_change(_) ->
  %% TODO delete certs?
  pass.
