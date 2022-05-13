#!/bin/bash

set -e

COMPONENT_NAME='aws.greengrass.clientdevices.mqtt.Emqx'
VERSION='NEXT_PATCH'

# Skip build as it is expensive. Just log instead. For now.
echo "To build, first run `python3 -u -m bin`
#python3 -u -m bin

mkdir -p greengrass-build/artifacts/${COMPONENT_NAME}/${VERSION}
mkdir -p greengrass-build/recipes

cp build/emqx.zip greengrass-build/artifacts/${COMPONENT_NAME}/${VERSION}/ && cp recipe.json greengrass-build/recipes/
