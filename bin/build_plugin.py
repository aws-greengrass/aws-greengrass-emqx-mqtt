#  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
#  SPDX-License-Identifier: Apache-2.0

import json
import os
import shutil
import subprocess
import datetime

REBAR = 'rebar3'


def build_plugin(context) -> None:
    print("Building AWS Greengrass Auth Plugin")
    plugin_libs = [os.path.abspath('_build/driver_lib')]
    if os.name == 'nt':
        plugin_libs.extend([
            os.path.abspath('patches/msvcp140.dll'),
            os.path.abspath('patches/vcruntime140.dll'),
            os.path.abspath('patches/vcruntime140_1.dll'),
        ])

    os.chdir(context.auth_plugin_name)

    for lib in plugin_libs:
        if os.path.isdir(lib):
            shutil.copytree(lib, 'priv', dirs_exist_ok=True)
        else:
            shutil.copy(lib, 'priv')

    try:
        os.symlink(os.path.join(context.original_path, 'emqx', REBAR), REBAR)
    except FileExistsError:
        pass

    plugin_build_cmd = f'{REBAR} release' if os.name == 'nt' else f'./{REBAR} release'
    plugin_build_env = dict(os.environ, BUILD_WITHOUT_ROCKSDB="true")

    if os.name == 'nt':
        vcvars_path, arch = context.get_vcvars_path()
        plugin_build_cmd = f'call "{vcvars_path}" {arch} && {plugin_build_cmd}'

    subprocess.check_call(
        plugin_build_cmd,
        env=plugin_build_env,
        shell=True
    )

    plugin_name_version = f'{context.auth_plugin_name}-{context.auth_plugin_version}'

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
        "git_ref": subprocess.check_output('git rev-parse HEAD', shell=True).decode('utf-8').strip(),
        "metadata_vsn": "0.1.0",
        "name": context.auth_plugin_name,
        "rel_apps": [plugin_name_version],
        "rel_vsn": context.auth_plugin_version,
        "repo": "https://github.com/aws-greengrass/aws-greengrass-emqx-mqtt"
    }

    os.chdir(context.original_path)

    # Plugin structure in the released EMQX looks like
    # plugins
    # |--> pluginName-version
    #      |--> release.json
    #      |--> pluginName-version
    #           |--> ebin, priv, src
    plugin_release_path = \
        f"{context.auth_plugin_name}/_build/default/rel/{context.auth_plugin_name}/lib/{plugin_name_version}"
    shutil.copytree(src=plugin_release_path,
                    dst=os.path.join('emqx/_build/emqx/rel/emqx/plugins', plugin_name_version, plugin_name_version),
                    dirs_exist_ok=True,
                    symlinks=False)
    with open(os.path.join('emqx/_build/emqx/rel/emqx/plugins', plugin_name_version, 'release.json'), "w") as w:
        json.dump(release_info, w, indent=4)
