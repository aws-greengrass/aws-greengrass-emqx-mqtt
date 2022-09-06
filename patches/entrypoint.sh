#!/bin/sh
#
# Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0
#

set -e # quit on any errors

/opt/emqx/lib/aws_greengrass_emqx_auth-1.0.0/priv/write_config

exec "$@"
