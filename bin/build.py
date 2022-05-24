#  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
#  SPDX-License-Identifier: Apache-2.0
import pathlib
import shutil
import subprocess

import os
import sys
import wget

from .package import do_patch


def change_dir_permissions_recursive(path, mode):
    for root, dirs, files in os.walk(path, topdown=False):
        for dir in [os.path.join(root,d) for d in dirs]:
            os.chmod(dir, mode)
    for file in [os.path.join(root, f) for f in files]:
        os.chmod(file, mode)


def main():
    # Quick mode only builds our own plugin C++ code and puts it into the emqx zip file.
    # Run a full build before doing a quick build.
    quick_mode = False
    if len(sys.argv) == 2 and sys.argv[1] == "quick":
        print("Quick mode! Portions of the build will be skipped")
        quick_mode = True
    test_mode = False
    if len(sys.argv) == 2 and sys.argv[1] == "test":
        print("Test mode! Portions of the build will be skipped")
        test_mode = True
    current_abs_path = os.path.abspath(os.getcwd())

    release_type = "RelWithDebInfo"
    config = ""
    if os.name == "nt":
        config = f"--config {release_type}"

    if not quick_mode and not test_mode:
        print("Pulling all submodules recursively")
        subprocess.check_call("git submodule update --init --recursive", shell=True)

        # Copy development IPC SDK into C++ SDK submodule
        print("Copying development SDK")
        shutil.copytree("patches/greengrass_ipc", "aws-iot-device-sdk-cpp-v2/greengrass_ipc", dirs_exist_ok=True)
        shutil.copytree("patches/event_stream", "aws-iot-device-sdk-cpp-v2/eventstream_rpc", dirs_exist_ok=True)
        print("Building IPC SDK")
        pathlib.Path("_build_sdk").mkdir(parents=True, exist_ok=True)
        os.chdir("_build_sdk")
        # Build SDK
        subprocess.check_call(f"cmake -DCMAKE_INSTALL_PREFIX=. -DCMAKE_BUILD_TYPE=\"{release_type}\""
                              " -DBUILD_TESTING=OFF ../aws-iot-device-sdk-cpp-v2", shell=True)
        subprocess.check_call(f"cmake --build . --target install {config}", shell=True)

    # Build plugin
    print("Building native plugin")
    os.chdir(current_abs_path)
    pathlib.Path("_build").mkdir(parents=True, exist_ok=True)
    os.chdir("_build")

    enable_unit_test_flag = ""
    if not quick_mode or test_mode:
        # enabled by default
        print("Enabling unit tests")
        if os.name != 'nt':
            # install lcov on non-windows for coverage
            zip_name = "lcov-master.zip"
            dir_name = "lcov-master"
            if os.path.isfile(zip_name):
                os.remove(zip_name)
            if os.path.isdir(dir_name):
                shutil.rmtree(dir_name)
            wget.download("https://github.com/linux-test-project/lcov/archive/master.zip")
            shutil.unpack_archive(zip_name, ".")
            os.chdir(dir_name)
            change_dir_permissions_recursive("bin", 0o777)
            subprocess.check_call("sudo make install", shell=True)
    else:
        enable_unit_test_flag = "-DBUILD_TESTS=OFF"

    os.chdir(current_abs_path)
    os.chdir("_build")

    generator = ""
    if os.name == 'nt':
        print("Setting generator for Windows")
        generator = "-A x64"

    clang_tidy = ""
    if (shutil.which("clang-tidy")) is not None:
        clang_tidy = "-DCLANG_TIDY=1"
    subprocess.check_call(f"cmake {generator} {enable_unit_test_flag} -DCMAKE_BUILD_TYPE=\"{release_type}\""
                          f" {clang_tidy} -DCMAKE_PREFIX_PATH={current_abs_path}/_build_sdk ../port_driver/",
                          shell=True)
    # Run format the code
    if shutil.which("clang-format") is not None:
        print("Reformatting code using clang-format")
        subprocess.check_call("cmake --build . --target clangformat", shell=True)
        # Fail the build on GitHub if there are format changes needed
        if os.getenv("GITHUB_ACTIONS") == "true":
            try:
                subprocess.check_call("git diff --exit-code", shell=True,
                                      stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            except subprocess.CalledProcessError:
                print("Clang format check failed")
                sys.exit(1)
    else:
        print("clang-format not found, won't format or check format of files. Install using `brew install "
              "clang-format`, `sudo apt install -y clang-format`, or `choco install -y llvm`")

    subprocess.check_call(f"cmake --build . {config}", shell=True)
    if not quick_mode or test_mode:
        print("Running unit tests")
        if os.name != 'nt':
            # run UTs with coverage
            subprocess.check_call("cmake --build . --target port_driver_unit_tests-coverage", shell=True)
        else:
            subprocess.check_call(f".\\bin\\{release_type}\\port_driver_unit_tests.exe", shell=True)
    os.chdir(current_abs_path)
    # Put the output driver library into priv which will be built into the EMQ X distribution bundle
    shutil.copytree("_build/driver_lib", "priv", dirs_exist_ok=True)

    if test_mode:
        return

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
            vs_paths = {("C:\\Program Files (x86)\\Microsoft Visual "
                        "Studio\\2019\\Enterprise\\VC\\Auxiliary\\Build\\vcvarsall.bat", "x86_amd64"),
                        ("C:\\Program Files (x86)\\Microsoft Visual "
                         "Studio\\2019\\Community\\VC\\Auxiliary\\Build\\vcvarsall.bat", "x86_amd64"),
                        ("C:\\Program Files\\Microsoft Visual "
                         "Studio\\2022\\Community\\VC\\Auxiliary\\Build\\vcvarsall.bat", "x86_amd64"),
                        ("C:\\Program Files\\Microsoft Visual "
                         "Studio\\2022\\Community\\VC\\Auxiliary\\Build\\vcvarsall.bat", "x86_amd64"),
                        ("C:\\Program Files (x86)\\Microsoft Visual "
                        "Studio\\2022\\BuildTools\\Common7\\Tools\\VsDevCmd.bat", "-arch=amd64")}
            for vs_path in vs_paths:
                vs_arch = vs_path[1]
                vs_path = vs_path[0]
                if os.path.exists(vs_path):
                    break
            else:
                raise FileNotFoundError("Unable to find where VS is installed!")
            additional_env_var_script = f'call "{vs_path}" {vs_arch}'
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

        add = {"emqx/etc/plugins/aws_greengrass_emqx_auth.conf": "etc/aws_greengrass_emqx_auth.conf",
               "emqx/etc/acl.conf": "patches/acl.conf",
               "emqx/etc/emqx.conf": "patches/emqx.conf"}

        # On Windows, bundle in msvc runtime 120
        if os.name == 'nt':
            add[f"emqx/erts-{erts_version}/bin/msvcr120.dll"] = "patches/msvcr120.dll"
            add[f"emqx/lib/aws_greengrass_emqx_auth-1.0.0/priv/msvcp140.dll"] = "patches/msvcp140.dll"
            add[f"emqx/lib/aws_greengrass_emqx_auth-1.0.0/priv/vcruntime140.dll"] = "patches/vcruntime140.dll"
            add[f"emqx/lib/aws_greengrass_emqx_auth-1.0.0/priv/vcruntime140_1.dll"] = "patches/vcruntime140_1.dll"
        do_patch("build/emqx.zip", erts_version=erts_version, add=add)

    os.chdir(current_abs_path)
    if quick_mode:
        subprocess.check_call("rebar3 compile", shell=True)
        shutil.copytree("_build/default/lib/aws_greengrass_emqx_auth/ebin",
                        "build/emqx/lib/aws_greengrass_emqx_auth-1.0.0/ebin", dirs_exist_ok=True),

    # If EMQ X is already unzipped into build, then update our built plugin native code so we don't need to unzip again
    if os.path.exists("build/emqx/lib/aws_greengrass_emqx_auth-1.0.0/priv"):
        shutil.copytree("_build/driver_lib", "build/emqx/lib/aws_greengrass_emqx_auth-1.0.0/priv", dirs_exist_ok=True)
