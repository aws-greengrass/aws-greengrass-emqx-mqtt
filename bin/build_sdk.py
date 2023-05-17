#  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
#  SPDX-License-Identifier: Apache-2.0

import os
import pathlib
import subprocess


def build_sdk(context):
    print("Pulling all submodules recursively")
    subprocess.check_call("git submodule update --init --recursive", shell=True)

    print("Building IPC SDK")
    pathlib.Path("_build_sdk").mkdir(parents=True, exist_ok=True)
    os.chdir("_build_sdk")

    subprocess.check_call(f"cmake -DCMAKE_INSTALL_PREFIX=. -DCMAKE_BUILD_TYPE=\"{context.cmake_build_type}\""
                          " -DBUILD_TESTING=OFF ../aws-iot-device-sdk-cpp-v2", shell=True)
    subprocess.check_call(f"cmake --build . --target install {context.cmake_config_arg()}", shell=True)
