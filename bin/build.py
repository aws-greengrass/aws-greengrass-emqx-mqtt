#  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
#  SPDX-License-Identifier: Apache-2.0

import argparse
import os
from .utils import find_vcvars_path


# NOTE: imports are made lazily, for purposes of image layer caching for this repo's Dockerfile.

class Context:
    """
    Context that's available to all stages of the build.
    """

    def __init__(self) -> None:
        # build modes
        self.quick = False
        self.test_only = False
        self.no_test = False
        self.sdk_only = False
        self.emqx_only = False
        self.port_driver_only = False
        self.plugin_only = False
        self.package_only = False
        self.no_sdk = False
        # constants
        self.cmake_build_type = 'RelWithDebInfo'
        self.auth_plugin_name = 'gg'
        self.auth_plugin_version = '1.0.0'  # TODO get this dynamically
        self.original_path = os.path.abspath(os.getcwd())
        # not available until emqx has been built
        self._erts_version = None
        # not available until emqx has been built
        self._emqx_version = None

    def cmake_config_arg(self):
        return f'--config {self.cmake_build_type}' if os.name == 'nt' else ''

    def get_erts_version(self):
        if self._erts_version is None:
            from .build_emqx import load_erts_version
            prev_dir = os.getcwd()
            os.chdir(os.path.join(self.original_path, 'emqx'))
            self._erts_version = load_erts_version()
            os.chdir(prev_dir)
        return self._erts_version

    def get_emqx_version(self):
        if self._emqx_version is None:
            from .build_emqx import load_emqx_version
            prev_dir = os.getcwd()
            os.chdir(os.path.join(self.original_path, 'emqx'))
            self._emqx_version = load_emqx_version()
            os.chdir(prev_dir)
        return self._emqx_version

    def get_vcvars_path(self):
        return find_vcvars_path()


class Stage:
    def __init__(self, name, only=False, skip=False, quick=False, testable=False, run=None) -> None:
        self.name = name
        self.only = only
        self.skip = skip
        self.quick = quick
        self.testable = testable
        self.run = run

    def __str__(self) -> str:
        return self.name


def main():
    context = Context()

    class StoreInContextAction(argparse.Action):
        """
        An argparse action that takes a flag arg, like --emqx-only,
        and sets the corresponding field in Context to True (e.g. context.emqx_only = True)
        """
        def __init__(self, option_strings, dest, **kwargs):
            super().__init__(option_strings, dest, 0, True, **kwargs)

        def __call__(self, parser, namespace, values, option_string=None):
            mode = option_string[2:].replace('-', '_')
            setattr(context, mode, True)

    action = StoreInContextAction

    parser = argparse.ArgumentParser()
    parser.add_argument('--quick', action=action)
    parser.add_argument('--test-only', action=action)
    parser.add_argument('--no-test', action=action)
    parser.add_argument('--sdk-only', action=action)
    parser.add_argument('--emqx-only', action=action)
    parser.add_argument('--port-driver-only', action=action)
    parser.add_argument('--plugin-only', action=action)
    parser.add_argument('--package-only', action=action)
    parser.add_argument('--no-sdk', action=action)
    # set build modes in context
    parser.parse_args()

    if context.quick:
        print("Quick mode! Portions of the build will be skipped")
    if context.test_only:
        print("Test mode! Portions of the build will be skipped")
    if context.emqx_only:
        print("EMQX-only mode! Only EMQX will be built")

    def build_sdk_action():
        from .build_sdk import build_sdk
        build_sdk(context)

    def build_port_driver_action():
        from .build_port_driver import build_port_driver
        build_port_driver(context)

    def build_emqx_action():
        from .build_emqx import build_emqx
        build_emqx(context)

    def build_plugin_action():
        from .build_plugin import build_plugin
        build_plugin(context)

    def package_action():
        from .package import package
        package(context)

    stages = [
        Stage(
            name='sdk',
            skip=context.no_sdk,
            only=context.sdk_only,
            run=build_sdk_action,
        ),
        Stage(
            name='port-driver',
            only=context.port_driver_only,
            testable=True,
            run=build_port_driver_action,
        ),
        Stage(
            name='emqx',
            only=context.emqx_only,
            run=build_emqx_action,
        ),
        Stage(
            name='plugin',
            only=context.plugin_only,
            quick=True,
            run=build_plugin_action,
        ),
        Stage(
            name='package',
            only=context.package_only,
            quick=True,
            run=package_action,
        )
    ]

    def next_stage():
        done = False
        while stages and not done:
            stage = stages.pop(0)
            if stage.skip:
                continue
            if context.quick and not stage.quick:
                continue
            if context.test_only and not stage.testable:
                continue
            # skip if another stage should only execute
            if any(map(lambda stg: stg.only, stages)):
                continue
            # check if we're in a terminal stage
            if stage.only:
                done = True
            yield stage

    for s in next_stage():
        os.chdir(context.original_path)
        print(f'Beginning build stage: {s}')
        s.run()
