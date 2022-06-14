#  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
#  SPDX-License-Identifier: Apache-2.0
import io
import os
import re
import sys
import tempfile
import zipfile

import patch as pypatch

# plugins to disable in data/loaded_plugins
disabled_emqx_plugins = [
    "emqx_management"
]
# matches an entry in data/loaded_plugins
# group 0: whole entry
# group 1: plugin name
# group 2: enabled bool
loaded_plugin_pattern = r'{\s*(.*)\s*,\s*(true|false)\s*}\.'

def update_zip(zipname, updates, add={}):
    tmpfd, tmpname = tempfile.mkstemp(dir=os.path.dirname(zipname))
    os.close(tmpfd)

    originals = {}
    with zipfile.ZipFile(zipname, 'r') as zin:
        with zipfile.ZipFile(tmpname, 'w') as zout:
            zout.comment = zin.comment
            for item in zin.infolist():
                if item.filename not in updates.keys() and item.filename not in add.keys():
                    zout.writestr(item, zin.read(item.filename))
                else:
                    originals[item.filename] = zin.read(item.filename)

    os.remove(zipname)
    os.rename(tmpname, zipname)

    with zipfile.ZipFile(zipname, mode='a', compression=zipfile.ZIP_DEFLATED) as zf:
        for k, v in originals.items():
            if k in updates.keys():
                zf.writestr(k, updates[k](v))
        for dest, src in add.items():
            zf.write(src, dest)


def patch(original, patch_file):
    pset = pypatch.fromfile(patch_file)
    out = io.BytesIO()
    for r in pset.patch_stream(io.BytesIO(original), pset.items[0].hunks):
        out.write(r)
    return out.getvalue().decode()


def append(original, addition):
    return original.decode('utf-8') + addition


def do_patch(zip_path, erts_version="11.0", add=None):

    def disable_plugins(plugins_to_disable=[]):
        if not plugins_to_disable:
            return

        def _replace_plugin_entry(match):
            if match.group(1) not in plugins_to_disable:
                # no replacement
                return match.group(0)
            return f'{{{match.group(1)}, false}}.'

        def _disable_plugins(contents):
            return re.sub(loaded_plugin_pattern, _replace_plugin_entry, contents.decode('utf-8'))

        update_zip(
            zipname=zip_path,
            updates={
                "emqx/data/loaded_plugins": _disable_plugins
            }
        )

    # ini file is only for windows, but we'll just throw it in no matter what. Use \r\n for windows line endings
    with open("build/erl.ini", "w") as erl_ini:
        erl_ini.writelines(["[erlang]\r\n",
                            f"Bindir=.\\\\erts-{erts_version}\\\\bin\r\n",
                            "Progname=erl\r\n",
                            "Rootdir=.\\\\\r\n"
                            ])
    
    if add is None:
        add = {}
    add[f"emqx/erts-{erts_version}/bin/erl.ini"] = "build/erl.ini"

    update_zip(zipname=zip_path, updates={
        "emqx/bin/emqx.cmd": lambda o: patch(o, "patches/emqx.cmd.diff"),
        "emqx/bin/emqx": lambda o: patch(o, "patches/emqx.diff"),
        "emqx/bin/emqx_ctl.cmd": lambda o: patch(o, "patches/emqx_ctl.diff"),
        "emqx/data/loaded_plugins": lambda o: append(o, "{aws_greengrass_emqx_auth, true}.\n"),
    }, add=add)

    disable_plugins(plugins_to_disable=disabled_emqx_plugins)


if __name__ == "__main__":
    do_patch(zip_path=sys.argv[1])
