#  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
#  SPDX-License-Identifier: Apache-2.0

import os
import pathlib
import shutil
import subprocess
import sys

import wget


LCOV_VERSION = 1.16


def should_test(context):
    return not (context.quick or context.no_test or os.name == "nt" or context.emqx_only) or context.test_only


def change_dir_permissions_recursive(path, mode):
    for root, dirs, files in os.walk(path, topdown=False):
        for dir in [os.path.join(root, d) for d in dirs]:
            os.chmod(dir, mode)
        for file in [os.path.join(root, f) for f in files]:
            os.chmod(file, mode)


def build_port_driver(context) -> None:
    print("Building native plugin")
    pathlib.Path("_build").mkdir(parents=True, exist_ok=True)
    os.chdir("_build")

    enable_unit_test_flag = "-DBUILD_TESTS=OFF"
    run_tests = should_test(context)
    if run_tests:
        print("Enabling unit tests")
        enable_unit_test_flag = "-DBUILD_TESTS=ON"
        # install lcov on non-windows for coverage
        zip_name = f"lcov-{LCOV_VERSION}.zip"
        dir_name = f"lcov-{LCOV_VERSION}"
        if os.path.isfile(zip_name):
            os.remove(zip_name)
        if os.path.isdir(dir_name):
            shutil.rmtree(dir_name)
        wget.download(f"https://github.com/linux-test-project/lcov/archive/refs/tags/v{LCOV_VERSION}.zip")
        shutil.unpack_archive(zip_name, ".")
        prev_dir = os.getcwd()
        os.chdir(dir_name)
        change_dir_permissions_recursive("bin", 0o777)
        subprocess.check_call("sudo make install", shell=True)
        os.chdir(prev_dir)

    generator = ""
    if os.name == 'nt':
        print("Setting generator for Windows")
        generator = "-A x64"

    clang_tidy = ""
    if (shutil.which("clang-tidy")) is not None:
        clang_tidy = "-DCLANG_TIDY=1"

    subprocess.check_call(f"cmake {generator} {enable_unit_test_flag} -DCMAKE_BUILD_TYPE=\"{context.cmake_build_type}\""
                          f" {clang_tidy} -DCMAKE_PREFIX_PATH={context.original_path}/_build_sdk ../port_driver/",
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

    subprocess.check_call(f"cmake --build . {context.cmake_config_arg()}", shell=True)
    if run_tests:
        print("Running unit tests")
        subprocess.check_call("cmake --build . --target port_driver_unit_tests-coverage", shell=True)
