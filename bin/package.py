#  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
#  SPDX-License-Identifier: Apache-2.0
import io
import os
import pathlib
import shutil
import sys
import tempfile
import zipfile

import patch as pypatch


def update_zip(zipname, updates, add):
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


def do_patch(zip_path, add=None):
    if add is None:
        add = {}
    add["emqx/THIRD-PARTY-LICENSES"] = "THIRD-PARTY-LICENSES"

    update_zip(
        zipname=zip_path,
        updates={
            "emqx/bin/emqx.cmd": lambda o: patch(o, "patches/emqx.cmd.diff"),
            "emqx/bin/emqx": lambda o: patch(o, "patches/emqx.diff"),
        },
        add=add
    )


def package(context):
    print("Zipping EMQ X")
    pathlib.Path("build").mkdir(parents=True, exist_ok=True)
    try:
        os.remove("build/emqx.zip")
    except FileNotFoundError:
        pass
    try:
        os.remove(f"emqx/_build/emqx/rel/emqx/emqx-{context.get_emqx_version()}.tar.gz")
    except FileNotFoundError:
        pass
    shutil.make_archive("build/emqx", "zip", "emqx/_build/emqx/rel")

    print("Patching EMQ X")
    add = {
        "emqx/etc/acl.conf": "patches/acl.conf",
        "emqx/etc/emqx.conf": "patches/emqx.conf"
    }
    # On Windows, bundle in msvc runtime 120
    if os.name == 'nt':
        add[f"emqx/erts-{context.get_erts_version()}/bin/msvcr120.dll"] = "patches/msvcr120.dll"
    do_patch("build/emqx.zip", add=add)


if __name__ == "__main__":
    do_patch(zip_path=sys.argv[1])
