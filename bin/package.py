#  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
#  SPDX-License-Identifier: Apache-2.0
import io
import os
import zipfile
import tempfile
import shutil
from pathlib import Path
import urllib.request

try:
    import patch as pypatch
except ModuleNotFoundError or NameError:
    import pip
    pip.main(['install', "patch"])
    import patch as pypatch

EMQX_VERSION = "4.3.12"

EMQX_DOWNLOAD_BASE = f"https://www.emqx.com/en/downloads/broker/{EMQX_VERSION}/emqx-PLATFORM-{EMQX_VERSION}CPU.zip"
OUTPUT_DIR = "target"

shutil.rmtree(OUTPUT_DIR, ignore_errors=True)
os.makedirs(OUTPUT_DIR, exist_ok=True)


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


for platform, cpu in [("windows", ""), ("ubuntu20.04", "-amd64")]:
    zip_path = str(Path(OUTPUT_DIR).joinpath(f"emqx-{platform}{cpu}.zip"))
    urllib.request.urlretrieve(EMQX_DOWNLOAD_BASE.replace("PLATFORM", platform).replace("CPU", cpu),
                               zip_path)
    add = {}
    if platform == "windows":
        add["emqx/erts-11.0/bin/msvcr120.dll"] = "patches/msvcr120.dll"
    update_zip(zipname=zip_path, updates={
        "emqx/erts-11.0/bin/erl.ini": lambda o: patch(o, "patches/erl.diff"),
        "emqx/bin/emqx.cmd": lambda o: patch(o, "patches/emqx.diff"),
        "emqx/bin/emqx_ctl.cmd": lambda o: patch(o, "patches/emqx_ctl.diff")
    }, add=add)
