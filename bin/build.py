#  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
#  SPDX-License-Identifier: Apache-2.0
import pathlib
import shutil
import subprocess

import os
import sys

from .package import do_patch


def main():
    # Quick mode only builds our own plugin C++ code and puts it into the emqx zip file.
    # Run a full build before doing a quick build.
    quick_mode = False
    if len(sys.argv) == 2 and sys.argv[1] == "quick":
        print("Quick mode! Portions of the build will be skipped")
        quick_mode = True
    current_abs_path = os.path.abspath(os.getcwd())

    if not quick_mode:
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
    os.chdir(current_abs_path)
    print("Building native plugin")
    run_unit_test = (os.name != 'nt')
    enable_unit_test = ""
    if run_unit_test:
        # enabled by default
        print("Enabling unit tests")
        # install lcov
        subprocess.check_call("wget 'https://github.com/linux-test-project/lcov/archive/master.zip'", shell=True)
        subprocess.check_call("unzip master.zip", shell=True)
        os.chdir("lcov-master")
        subprocess.check_call("sudo make install", shell=True)
    else:
        enable_unit_test = "-DBUILD_TESTS=OFF"

    os.chdir(current_abs_path)
    pathlib.Path("_build").mkdir(parents=True, exist_ok=True)
    os.chdir("_build")

    generator = ""
    if os.name == 'nt':
        print("Setting generator for Windows")
        generator = "-A x64"

    subprocess.check_call(f"cmake {generator} {enable_unit_test} -DCMAKE_PREFIX_PATH={current_abs_path}/_build_sdk ../port_driver/",
                          shell=True)
    subprocess.check_call("cmake --build .", shell=True)
    if run_unit_test:
        print("Running unit tests with coverage")
        subprocess.check_call("cmake --build . --target port_driver_unit_tests-coverage", shell=True)
    os.chdir(current_abs_path)
    # Put the output driver library into priv which will be built into the EMQ X distribution bundle
    shutil.copytree("_build/driver_lib", "priv", dirs_exist_ok=True)

    if not quick_mode:
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
        if os.name == 'nt':
            print("Setting additional env var for Windows")
            vs_paths = {"C:\\Program Files (x86)\\Microsoft Visual "
                        "Studio\\2019\\Enterprise\\VC\\Auxiliary\\Build\\vcvarsall.bat",
                        "C:\\Program Files (x86)\\Microsoft Visual "
                        "Studio\\2019\\Community\\VC\\Auxiliary\\Build\\vcvarsall.bat",
                        "C:\\Program Files\\Microsoft Visual "
                        "Studio\\2022\\Community\\VC\\Auxiliary\\Build\\vcvarsall.bat",
                        "C:\\Program Files\\Microsoft Visual "
                        "Studio\\2022\\Community\\VC\\Auxiliary\\Build\\vcvarsall.bat"}
            for vs_path in vs_paths:
                if os.path.exists(vs_path):
                    break
            else:
                raise FileNotFoundError("Unable to find where VS is installed!")
            additional_env_var_script = f'call "{vs_path}" x86_amd64'
            subprocess.check_call(f"{additional_env_var_script} && make -j", shell=True,
                                  env=dict(os.environ, EMQX_EXTRA_PLUGINS="aws_greengrass_emqx_auth"))
        else:
            subprocess.check_call(f"make -j", shell=True,
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

    os.chdir(current_abs_path)
    if quick_mode:
        subprocess.check_call("rebar3 compile", shell=True)
        shutil.copytree("_build/default/lib/aws_greengrass_emqx_auth/ebin",
                        "build/emqx/lib/aws_greengrass_emqx_auth-1.0.0/ebin", dirs_exist_ok=True),

    # If EMQ X is already unzipped into build, then update our built plugin native code so we don't need to unzip again
    if os.path.exists("build/emqx/lib/aws_greengrass_emqx_auth-1.0.0/priv"):
        shutil.copytree("_build/lib", "build/emqx/lib/aws_greengrass_emqx_auth-1.0.0/priv", dirs_exist_ok=True)
