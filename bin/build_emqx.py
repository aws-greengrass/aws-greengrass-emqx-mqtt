#  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
#  SPDX-License-Identifier: Apache-2.0

import os
import shutil
import subprocess


def build_emqx(context):
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
        os.remove(f"{context.original_path}/emqx/apps/aws_greengrass_emqx_auth")
    except FileNotFoundError:
        pass
    os.symlink(context.original_path, f"{context.original_path}/emqx/apps/aws_greengrass_emqx_auth",
               target_is_directory=True)

    # Since 5.0.22, peercert was removed from EMQX plugin hooks.
    # Until we find a workaround or it's added back, we'll simply
    # revert the change.
    # https://github.com/emqx/emqx/pull/10243/commits/07ac2cd57aaba0f2e1776ad36ddbed85475a753a
    subprocess.check_call(f'git revert --no-commit 07ac2cd57aaba0f2e1776ad36ddbed85475a753a', shell=True)

    print("Building EMQ X")
    emqx_build_cmd = 'make -j'
    emqx_build_env = dict(os.environ, BUILD_WITHOUT_ROCKSDB="true")

    if os.name == 'nt':
        # ensure visual studio environment is set properly
        # when building on Windows
        vcvars_path, arch = context.get_vcvars_path()
        emqx_build_cmd = f'call "{vcvars_path}" {arch} && {emqx_build_cmd}'

    # build emqx
    subprocess.check_call(
        emqx_build_cmd,
        env=emqx_build_env,
        shell=True
    )

    # Remove erl.ini. This, paired with not cd-ing in emqx.cmd,
    # will allow erlang to use the greengrass work dir as its
    # working directory.  We want this because in Windows,
    # we moved the etc and data dirs to the work dir.
    #
    # Ideally this removal should happen during do_patch,
    # but ZipFile doesn't support the removal of files.
    try:
        os.remove(f'_build/emqx/rel/emqx/erts-{context.get_erts_version()}/bin/erl.ini')
    except FileNotFoundError:
        pass
    # Remove the default certs that ship with EMQX just to be sure they can't be used for any reason
    shutil.rmtree("_build/emqx/rel/emqx/etc/certs")


def load_erts_version():
    erts_version = None
    with open('_build/emqx/rel/emqx/releases/emqx_vars', 'r') as file:
        for l in file.readlines():
            if l.startswith("ERTS_VSN"):
                erts_version = l.split("ERTS_VSN=")[1].strip().strip("\"")
    if erts_version is None:
        raise ValueError("Didn't find ERTS version")
    print("ERTS version", erts_version)
    return erts_version


def load_emqx_version():
    emqx_version = None
    with open('_build/emqx/rel/emqx/releases/emqx_vars', 'r') as file:
        for l in file.readlines():
            if l.startswith("REL_VSN"):
                emqx_version = l.split("REL_VSN=")[1].strip().strip("\"")
    if emqx_version is None:
        raise ValueError("Didn't find EMQX version")
    print("EMQX version", emqx_version)
    return emqx_version
