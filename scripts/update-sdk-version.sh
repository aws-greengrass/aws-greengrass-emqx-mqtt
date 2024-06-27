#!/usr/bin/env bash
set -e

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
git add aws-iot-device-sdk-cpp-v2

# bump the cache key in github actions so we don't accidently use the cached previous version of the sdk
ACTIONS_BUILD_FILE=.github/workflows/build.yml
SDK_RESTORE_TASK_ID=cache-sdk-restore
OLD_CACHE_KEY=$(yq eval ".jobs.build-native.steps[] | select(.id == \"${SDK_RESTORE_TASK_ID}\").with.key" "${ACTIONS_BUILD_FILE}")
if [[ -z "${OLD_CACHE_KEY}" ]]; then
  echo "SDK cache key not found in ${ACTIONS_BUILD_FILE}. Please update this script"
  exit 1
fi
OLD_CACHE_VERSION=$(echo "${OLD_CACHE_KEY}" | awk -F'-' '{print $3}')
NEW_CACHE_VERSION=$((OLD_CACHE_VERSION + 1))
UPDATED_BUILD_FILE_CONTENTS=$(yq eval "
    (.jobs.build-native.steps[] | select(.id == \"${SDK_RESTORE_TASK_ID}\").with.key) |= 
    sub(\"cache-sdk-${OLD_CACHE_VERSION}-\", \"cache-sdk-${NEW_CACHE_VERSION}-\")" \
    "${ACTIONS_BUILD_FILE}")
echo "${UPDATED_BUILD_FILE_CONTENTS}" > ${ACTIONS_BUILD_FILE}
git add "${ACTIONS_BUILD_FILE}"

# commit the changes
git commit -m "chore: update AWS IoT SDK to ${HASH}"

echo "Updated AWS IoT SDK to ${HASH}, please verify then push your changes"
