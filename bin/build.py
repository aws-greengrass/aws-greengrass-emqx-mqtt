#  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
#  SPDX-License-Identifier: Apache-2.0
import pathlib
import shutil
import subprocess

import os


def main():
    current_abs_path = os.path.abspath(os.getcwd())

    # Copy development IPC SDK into C++ SDK submodule
    print("Copying development SDK")
    shutil.copytree("patches/greengrass_ipc", "aws-iot-device-sdk-cpp-v2/greengrass_ipc", dirs_exist_ok=True)
    print("Building IPC SDK")
    pathlib.Path("_build_sdk").mkdir(parents=True, exist_ok=True)
    os.chdir("_build_sdk")
    # Build SDK
    subprocess.check_call("cmake -DCMAKE_INSTALL_PREFIX=. -DCMAKE_BUILD_TYPE=\"Debug\" -DBUILD_TESTING=OFF" +
                          " ../aws-iot-device-sdk-cpp-v2", shell=True)
    subprocess.check_call("cmake --build . --target install", shell=True)

    # Build plugin
    print("Building native plugin")
    os.chdir(current_abs_path)
    pathlib.Path("_build").mkdir(parents=True, exist_ok=True)
    os.chdir("_build")
    subprocess.check_call(f"cmake -DCMAKE_PREFIX_PATH={current_abs_path}/_build_sdk ../port_driver/", shell=True)
    subprocess.check_call("cmake --build .", shell=True)
    os.chdir(current_abs_path)

    print("Cloning EMQ X")
    subprocess.check_call("git clone https://github.com/emqx/emqx.git", shell=True)
    with open('emqx.commit', 'r') as file:
        emqx_commit_id = file.read().rstrip()
    os.chdir("emqx")
    subprocess.check_call(f"git reset --hard {emqx_commit_id}", shell=True)
    pathlib.Path("_checkouts").mkdir(parents=True, exist_ok=True)
    os.chdir(current_abs_path)
    shutil.copyfile(".github/emqx_plugins_patch", "emqx/lib-extra/plugins")

    print("Setting up EMQ X plugin checkout symlink")
    os.symlink(current_abs_path, f"{current_abs_path}/emqx/_checkouts/aws_greengrass_emqx_auth",
               target_is_directory=True)

    os.chdir("emqx")
    print("Building EMQ X")
    subprocess.check_call("make -j", shell=True,
                          env=dict(os.environ, EMQX_EXTRA_PLUGINS="aws_greengrass_emqx_auth"))


if __name__ == '__main__':
    main()
