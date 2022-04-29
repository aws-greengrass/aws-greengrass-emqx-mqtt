#  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
#  SPDX-License-Identifier: Apache-2.0
import io
import os
import zipfile
import tempfile
import shutil
from pathlib import Path
import urllib.request
import sys

try:
    import patch as pypatch
except ModuleNotFoundError or NameError:
    import pip
    pip.main(['install', "patch"])
    import patch as pypatch


def update_zip(zipname, updates, add):
    tmpfd, tmpname = tempfile.mkstemp(dir=os.path.dirname(zipname))
    os.close(tmpfd)

    originals = {}
    with zipfile.ZipFile(zipname, 'r') as zin:
        with zipfile.ZipFile(tmpname, 'w') as zout:
            zout.comment = zin.comment
            for item in zin.infolist():
                if item.filename not in updates.keys():
                    zout.writestr(item, zin.read(item.filename))
                else:
                    originals[item.filename] = zin.read(item.filename)

    os.remove(zipname)
    os.rename(tmpname, zipname)

    with zipfile.ZipFile(zipname, mode='a', compression=zipfile.ZIP_DEFLATED) as zf:
        for k, v in originals.items():
            zf.writestr(k, updates[k](v))
        for dest, src in add.items():
            zf.write(src, dest)


def patch(original, patch_file):
    pset = pypatch.fromfile(patch_file)
    out = io.BytesIO()
    for r in pset.patch_stream(io.BytesIO(original), pset.items[0].hunks):
        out.write(r)
    return out.getvalue().decode()


zip_path = sys.argv[1]
add = {}
update_zip(zipname=zip_path, updates={
    "emqx/erts-11.0/bin/erl.ini": lambda o: patch(o, "patches/erl.diff"),
    "emqx/bin/emqx.cmd": lambda o: patch(o, "patches/emqx.diff"),
    "emqx/bin/emqx_ctl.cmd": lambda o: patch(o, "patches/emqx_ctl.diff")
}, add=add)
