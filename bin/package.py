#  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
#  SPDX-License-Identifier: Apache-2.0
import os
import shutil
from pathlib import Path
import urllib.request

EMQX_VERSION = "4.3.11"

EMQX_DOWNLOAD_BASE = f"https://www.emqx.com/en/downloads/broker/{EMQX_VERSION}/emqx-PLATFORM-{EMQX_VERSION}CPU.zip"
OUTPUT_DIR = "target"

shutil.rmtree(OUTPUT_DIR, ignore_errors=True)
os.makedirs(OUTPUT_DIR, exist_ok=True)

for platform, cpu in [("windows", ""), ("ubuntu20.04", "-amd64")]:
    urllib.request.urlretrieve(EMQX_DOWNLOAD_BASE.replace("PLATFORM", platform).replace("CPU", cpu),
                               str(Path(OUTPUT_DIR).joinpath(f"emqx-{platform}{cpu}.zip")))
