
#  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
#  SPDX-License-Identifier: Apache-2.0

import os
from typing import Tuple


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
