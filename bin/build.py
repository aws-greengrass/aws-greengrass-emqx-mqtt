#  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
#  SPDX-License-Identifier: Apache-2.0
import pathlib
import shutil
import subprocess

import os

from .package import do_patch


def main():
    current_abs_path = os.path.abspath(os.getcwd())

    print("Pulling all submodules recursively")
    subprocess.check_call("git submodule update --init --recursive", shell=True)

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

    generator = ""
    if os.name == 'nt':
        print("Setting generator for Windows")
        generator = "-G \"Visual Studio 15 2017 Win64\""

    subprocess.check_call(f"cmake {generator} -DCMAKE_PREFIX_PATH={current_abs_path}/_build_sdk ../port_driver/", shell=True)
    subprocess.check_call("cmake --build .", shell=True)
    os.chdir(current_abs_path)
    # Put the output library into priv which will be built into the EMQ X distribution bundle
    shutil.copytree("_build/lib", "priv", dirs_exist_ok=True)

    print("Cloning EMQ X")
    try:
        subprocess.check_call("git clone https://github.com/emqx/emqx.git", shell=True)
    except subprocess.CalledProcessError as e:
        # Ignore EMQ X already cloned
        if e.returncode != 128:
            raise

    with open('emqx.commit', 'r') as file:
        emqx_commit_id = file.read().rstrip()
    os.chdir("emqx")
    subprocess.check_call(f"git reset --hard {emqx_commit_id}", shell=True)
    pathlib.Path("_checkouts").mkdir(parents=True, exist_ok=True)
    os.chdir(current_abs_path)
    shutil.copyfile(".github/emqx_plugins_patch", "emqx/lib-extra/plugins")

    print("Setting up EMQ X plugin checkout symlink")
    try:
        os.symlink(current_abs_path, f"{current_abs_path}/emqx/_checkouts/aws_greengrass_emqx_auth",
                   target_is_directory=True)
    except FileExistsError:
        pass

    os.chdir("emqx")
    print("Building EMQ X") 
    additional_env_var_script = "true"        
    if os.name == 'nt':
        print("Setting additional env var for Windows")
        additional_env_var_script = "vcvarsall.bat x86_amd64"
    subprocess.check_call(f"{additional_env_var_script} && make -j", shell=True,
                          env=dict(os.environ, EMQX_EXTRA_PLUGINS="aws_greengrass_emqx_auth"))

    os.chdir(current_abs_path)
    pathlib.Path("build").mkdir(parents=True, exist_ok=True)
    try:
        os.remove("build/emqx.zip")
    except FileNotFoundError:
        pass
    print("Zipping EMQ X")
    shutil.make_archive("build/emqx", "zip", "emqx/_build/emqx/rel")

    print("Patching EMQ X")
    erts_version = None
    with open('emqx/_build/emqx/rel/emqx/releases/emqx_vars', 'r') as file:
        for l in file.readlines():
            if l.startswith("ERTS_VSN"):
                erts_version = l.split("ERTS_VSN=")[1].strip().strip("\"")
    if erts_version is None:
        raise ValueError("Didn't find ERTS version")
    print("ERTS version", erts_version)

    add = {"emqx/etc/plugins/aws_greengrass_emqx_auth.conf": "etc/aws_greengrass_emqx_auth.conf"}

    # On Windows, bundle in msvc runtime 120
    if os.name == 'nt':
        add[f"emqx/erts-{erts_version}/bin/msvcr120.dll"] = "patches/msvcr120.dll"
    do_patch("build/emqx.zip", erts_version=erts_version, add=add)
