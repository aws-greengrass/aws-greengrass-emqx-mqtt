#  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
#  SPDX-License-Identifier: Apache-2.0

import argparse
import datetime
import json
import pathlib
import shutil
import subprocess

import os
import sys
from typing import Tuple

import wget

from .package import do_patch

AUTH_PLUGIN_NAME = 'aws_greengrass_emqx_auth'
AUTH_PLUGIN_VERSION = '1.0.0'  # TODO get this dynamically


def change_dir_permissions_recursive(path, mode):
    for root, dirs, files in os.walk(path, topdown=False):
        for dir in [os.path.join(root, d) for d in dirs]:
            os.chmod(dir, mode)
        for file in [os.path.join(root, f) for f in files]:
            os.chmod(file, mode)


def find_vcvars_path() -> Tuple[str, str]:
    vcvars_paths = {
        ("C:\\Program Files (x86)\\Microsoft Visual "
         "Studio\\2019\\Enterprise\\VC\\Auxiliary\\Build\\vcvarsall.bat", "x86_amd64"),
        ("C:\\Program Files (x86)\\Microsoft Visual "
         "Studio\\2019\\Community\\VC\\Auxiliary\\Build\\vcvarsall.bat", "x86_amd64"),
        ("C:\\Program Files\\Microsoft Visual "
         "Studio\\2022\\Community\\VC\\Auxiliary\\Build\\vcvarsall.bat", "x86_amd64"),
        ("C:\\Program Files\\Microsoft Visual "
         "Studio\\2022\\Community\\VC\\Auxiliary\\Build\\vcvarsall.bat", "x86_amd64"),
        ("C:\\Program Files (x86)\\Microsoft Visual "
         "Studio\\2022\\BuildTools\\Common7\\Tools\\VsDevCmd.bat", "-arch=amd64")
    }
    vcvars_path, arch = next(filter(lambda path: os.path.exists(path[0]), vcvars_paths), (None, None))
    if not vcvars_path:
        raise FileNotFoundError("vcvarsall.bat/VsDevCmd.bat not found, "
                                "please ensure visual studio is installed")
    return vcvars_path, arch


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--quick', action='store_true')
    parser.add_argument('--test-only', action='store_true')
    parser.add_argument('--no-test', action='store_true')
    parser.add_argument('--sdk-only', action='store_true')
    parser.add_argument('--no-sdk', action='store_true')

    args = parser.parse_args()
    # Quick mode only builds our own plugin C++ code and puts it into the emqx zip file.
    # Run a full build before doing a quick build.
    quick_mode = args.quick
    if quick_mode:
        print("Quick mode! Portions of the build will be skipped")
    test_mode = args.test_only
    if test_mode:
        print("Test mode! Portions of the build will be skipped")
    current_abs_path = os.path.abspath(os.getcwd())

    release_type = "RelWithDebInfo"
    config = ""
    if os.name == "nt":
        config = f"--config {release_type}"

    if not quick_mode and not test_mode and not args.no_sdk:
        print("Pulling all submodules recursively")
        subprocess.check_call("git submodule update --init --recursive", shell=True)

        print("Building IPC SDK")
        pathlib.Path("_build_sdk").mkdir(parents=True, exist_ok=True)
        os.chdir("_build_sdk")
        # Build SDK
        subprocess.check_call(f"cmake -DCMAKE_INSTALL_PREFIX=. -DCMAKE_BUILD_TYPE=\"{release_type}\""
                              " -DBUILD_TESTING=OFF ../aws-iot-device-sdk-cpp-v2", shell=True)
        subprocess.check_call(f"cmake --build . --target install {config}", shell=True)
        if args.sdk_only:
            return

    # Build plugin
    print("Building native plugin")
    os.chdir(current_abs_path)
    pathlib.Path("_build").mkdir(parents=True, exist_ok=True)
    os.chdir("_build")

    enable_unit_test_flag = "-DBUILD_TESTS=OFF"
    do_test = not (quick_mode or args.no_test or os.name == "nt") or test_mode
    if do_test:
        print("Enabling unit tests")
        enable_unit_test_flag = "-DBUILD_TESTS=ON"
        # install lcov on non-windows for coverage
        zip_name = "lcov-1.16.zip"
        dir_name = "lcov-1.16"
        if os.path.isfile(zip_name):
            os.remove(zip_name)
        if os.path.isdir(dir_name):
            shutil.rmtree(dir_name)
        wget.download("https://github.com/linux-test-project/lcov/archive/refs/tags/v1.16.zip")
        shutil.unpack_archive(zip_name, ".")
        os.chdir(dir_name)
        change_dir_permissions_recursive("bin", 0o777)
        subprocess.check_call("sudo make install", shell=True)

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
    if do_test:
        print("Running unit tests")
        # run UTs with coverage
        subprocess.check_call("cmake --build . --target port_driver_unit_tests-coverage", shell=True)
    os.chdir(current_abs_path)

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
        subprocess.check_call(f"git fetch -a -p", shell=True)
        subprocess.check_call(f"git reset --hard {emqx_commit_id}", shell=True)

        print("Setting up EMQ X plugin checkout symlink")
        try:
            # Remove existing symlink (if any) before linking
            os.remove(f"{current_abs_path}/emqx/apps/aws_greengrass_emqx_auth")
        except FileNotFoundError:
            pass
        os.symlink(current_abs_path, f"{current_abs_path}/emqx/apps/aws_greengrass_emqx_auth",
                   target_is_directory=True)

        # Since 5.0.22, peercert was removed from EMQX plugin hooks.
        # Until we find a workaround or it's added back, we'll simply
        # revert the change.
        # https://github.com/emqx/emqx/pull/10243/commits/07ac2cd57aaba0f2e1776ad36ddbed85475a753a
        subprocess.check_call(f'git revert --no-commit 07ac2cd57aaba0f2e1776ad36ddbed85475a753a', shell=True)

        print("Building EMQ X")
        emqx_build_cmd = 'make -j'
        emqx_build_env = dict(os.environ)

        if os.name == 'nt':
            # ensure visual studio environment is set properly
            # when building on Windows
            vcvars_path, arch = find_vcvars_path()
            emqx_build_cmd = f'call "{vcvars_path}" {arch} && {emqx_build_cmd}'
            # https://github.com/emqx/emqx/issues/8477
            emqx_build_env['BUILD_WITHOUT_ROCKSDB'] = 'true'

        # build emqx
        subprocess.check_call(
            emqx_build_cmd,
            env=emqx_build_env,
            shell=True
        )

    erts_version = None
    emqx_version = None
    with open('_build/emqx/rel/emqx/releases/emqx_vars', 'r') as file:
        for l in file.readlines():
            if l.startswith("ERTS_VSN"):
                erts_version = l.split("ERTS_VSN=")[1].strip().strip("\"")
            elif l.startswith("REL_VSN"):
                emqx_version = l.split("REL_VSN=")[1].strip().strip("\"")
    if erts_version is None:
        raise ValueError("Didn't find ERTS version")
    if emqx_version is None:
        raise ValueError("Didn't find EMQX version")
    print("ERTS version", erts_version)
    print("EMQX version", emqx_version)

    # Remove erl.ini. This, paired with not cd-ing in emqx.cmd,
    # will allow erlang to use the greengrass work dir as its
    # working directory.  We want this because in Windows,
    # we moved the etc and data dirs to the work dir.
    #
    # Ideally this removal should happen during do_patch,
    # but ZipFile doesn't support the removal of files.
    try:
        os.remove(f'_build/emqx/rel/emqx/erts-{erts_version}/bin/erl.ini')
    except FileNotFoundError:
        pass
    # Remove the default certs that ship with EMQX just to be sure they can't be used for any reason
    shutil.rmtree("_build/emqx/rel/emqx/etc/certs")

    os.chdir(current_abs_path)
    pathlib.Path("build").mkdir(parents=True, exist_ok=True)
    try:
        os.remove("build/emqx.zip")
    except FileNotFoundError:
        pass

    print("Building AWS Greengrass Auth Plugin")
    plugin_libs = [os.path.abspath('_build/driver_lib')]
    if os.name == 'nt':
        plugin_libs.extend([
            os.path.abspath('patches/msvcp140.dll'),
            os.path.abspath('patches/vcruntime140.dll'),
            os.path.abspath('patches/vcruntime140_1.dll'),
        ])

    os.chdir(AUTH_PLUGIN_NAME)

    for lib in plugin_libs:
        if os.path.isdir(lib):
            shutil.copytree(lib, 'priv', dirs_exist_ok=True)
        else:
            shutil.copy(lib, 'priv')

    plugin_build_cmd = 'make release'
    plugin_build_env = dict(os.environ)

    if os.name == 'nt':
        vcvars_path, arch = find_vcvars_path()
        plugin_build_cmd = f'call "{vcvars_path}" {arch} && {plugin_build_cmd}'
        plugin_build_env['BUILD_WITHOUT_ROCKSDB'] = 'true'

    subprocess.check_call(
        plugin_build_cmd,
        env=plugin_build_env,
        shell=True
    )

    release_info = {
        "authors": [
            "AWS Greengrass"
        ],
        "builder": {
            "contact": "",
            "name": "AWS IoT Greengrass",
            "website": "https://aws.amazon.com/greengrass/"
        },
        "built_on_otp_release": "24",
        "compatibility": {
            "emqx": "~> 5.0.4"
        },
        "date": str(datetime.date.today()),
        "description": "Plugin that enables EMQX to authenticate/authorize requests via Greengrass",
        "functionality": [
            "AuthN", "AuthZ"
        ],
        "git_ref": subprocess.check_output('git rev-parse HEAD').decode('utf-8').strip(),
        "metadata_vsn": "0.1.0",
        "name": AUTH_PLUGIN_NAME,
        "rel_apps": [
            f'{AUTH_PLUGIN_NAME}-{AUTH_PLUGIN_VERSION}'
        ],
        "rel_vsn": AUTH_PLUGIN_VERSION,
        "repo": "https://github.com/aws-greengrass/aws-greengrass-emqx-mqtt"
    }

    os.chdir(current_abs_path)

    try:
        os.remove(f"emqx/_build/emqx/rel/emqx/emqx-{emqx_version}.tar.gz")
    except FileNotFoundError:
        pass

    # Plugin structure in the released EMQX looks like
    # plugins
    # |--> pluginName-version
    #      |--> release.json
    #      |--> pluginName-version
    #           |--> ebin, priv, src
    plugin_release_path = f"{AUTH_PLUGIN_NAME}/_build/default/rel/{AUTH_PLUGIN_NAME}/lib/{AUTH_PLUGIN_NAME}-{AUTH_PLUGIN_VERSION}"
    if os.path.exists("build/emqx"):
        shutil.copytree(src=plugin_release_path,
                        dst=os.path.join('build/emqx/plugins', f'{AUTH_PLUGIN_NAME}-{AUTH_PLUGIN_VERSION}',
                                         f'{AUTH_PLUGIN_NAME}-{AUTH_PLUGIN_VERSION}'),
                        dirs_exist_ok=True, symlinks=False)
    shutil.copytree(src=plugin_release_path,
                    dst=os.path.join('emqx/_build/emqx/rel/emqx/plugins', f'{AUTH_PLUGIN_NAME}-{AUTH_PLUGIN_VERSION}',
                                     f'{AUTH_PLUGIN_NAME}-{AUTH_PLUGIN_VERSION}'),
                    dirs_exist_ok=True, symlinks=False)
    with open(os.path.join('emqx/_build/emqx/rel/emqx/plugins', f'{AUTH_PLUGIN_NAME}-{AUTH_PLUGIN_VERSION}',
                           'release.json'), "w") as w:
        json.dump(release_info, w, indent=4)

    if not quick_mode:
        print("Zipping EMQ X")
        shutil.make_archive("build/emqx", "zip", "emqx/_build/emqx/rel")

        print("Patching EMQ X")
        add = {
            "emqx/etc/acl.conf": "patches/acl.conf",
            "emqx/etc/emqx.conf": "patches/emqx.conf"
        }
        # On Windows, bundle in msvc runtime 120
        if os.name == 'nt':
            add[f"emqx/erts-{erts_version}/bin/msvcr120.dll"] = "patches/msvcr120.dll"
        do_patch("build/emqx.zip", add=add)
