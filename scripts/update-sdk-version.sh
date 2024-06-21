#!/usr/bin/env bash

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <commitish>"
    exit 1
fi

HASH=$1

# navigate to project root
SCRIPT_DIR=$(dirname "$(realpath "$0")")
cd "${SCRIPT_DIR}/.."

# update sdk dir and switch to the new commit
git submodule update --init --recursive
cd aws-iot-device-sdk-cpp-v2
git fetch
git checkout ${HASH}
cd -

# commit the changes
git add aws-iot-device-sdk-cpp-v2
git commit -m "chore: update aws iot sdk to ${HASH}"

echo "Updated repo to ${HASH}, please verify then push your changes"
