#!/bin/sh

set -e # quit on any errors

/opt/emqx/lib/aws_greengrass_emqx_auth-1.0.0/priv/config_writer

exec "$@"
