#  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
#  SPDX-License-Identifier: Apache-2.0

import os
import pathlib
import shutil
import subprocess


def build_sdk(context):
    print("Pulling all submodules recursively")
    git = shutil.which("git")
    subprocess.check_call([git, "submodule", "update", "--init", "--recursive"])

    print("Building IPC SDK")
    pathlib.Path("_build_sdk").mkdir(parents=True, exist_ok=True)
    os.chdir("_build_sdk")

    cmake = shutil.which("cmake")
    cmake_configure_args = [
        cmake,
        "-DCMAKE_INSTALL_PREFIX=.",
        f"-DCMAKE_BUILD_TYPE={context.cmake_build_type}",
        "-DBUILD_TESTING=OFF",
        "-DCMAKE_POSITION_INDEPENDENT_CODE=ON",
    ]
    cmake_configure_args.append("../aws-iot-device-sdk-cpp-v2")
    subprocess.check_call(cmake_configure_args)

    cmake_build_args = [
        cmake, "--build", ".", "--target", "install",
    ]
    cmake_build_args.extend(context.cmake_config_arg().split())

    build_env = dict(os.environ)
    if os.name == 'nt':
        # Disable warnings-as-errors for the vendored SDK on MSVC.
        # The SDK sets /WX per-target, and _CL_ is appended after all flags (last wins).
        build_env["_CL_"] = "/WX-"

    subprocess.check_call(cmake_build_args, env=build_env)
