#!/usr/bin/env python
"""
Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
SPDX-License-Identifier: Apache-2.0
"""
import argparse
import os
# semver without need for external dependency
import packaging.version
import shutil
import subprocess
import sys
import zipfile

SCRIPT_DIR = sys.path[0]
REPO_DIR = os.path.abspath(os.path.join(SCRIPT_DIR, '..'))
EMQX_ZIP = os.path.join(REPO_DIR, 'build', 'emqx.zip')

DEFAULT_GREENGRASS_ROOT = os.path.join(os.sep, 'greengrass', 'v2')


def stop_greengrass() -> None:
    if os.name == 'nt':
        subprocess.call(['net', 'stop', 'greengrass'])  # allow failure, service may already be stopped
    else:
        raise NotImplementedError('unix not yet supported')


def start_greengrass() -> None:
    if os.name == 'nt':
        subprocess.check_call(['net', 'start', 'greengrass'])
    else:
        raise NotImplementedError('unix not yet supported')


class ValidationException(Exception):
    pass


class MissingDirectoryException(ValidationException):

    def __init__(self, dirname):
        super().__init__(f'{dirname} is missing or empty, check that your '
                         f'greengrass installation is correct and you\'ve'
                         f' already done a cloud deployment with EMQX '
                         f'component at least once')


class LocalDeployer:

    def __init__(self, gg_root: str) -> None:
        self.gg_root = gg_root
        self.artifacts_unarchived = os.path.join(gg_root,
                                                 'packages',
                                                 'artifacts-unarchived',
                                                 'aws.greengrass.clientdevices.mqtt.EMQX')
        self.deploy_dir = self._locate_deploy_dir()

    def _locate_deploy_dir(self) -> str:
        def parse_semver(path: os.PathLike) -> packaging.version.Version:
            return packaging.version.parse(os.path.basename(path))

        unarchived_versions = next(os.walk(self.artifacts_unarchived))[1]
        if not unarchived_versions:
            raise MissingDirectoryException(dirname=self.artifacts_unarchived)
        latest_version = max(unarchived_versions, key=parse_semver)
        return os.path.join(self.artifacts_unarchived, latest_version)

    def _validate(self):
        if not os.path.exists(EMQX_ZIP):
            raise ValidationException(f'{EMQX_ZIP} is missing, run a build first: python -u -m bin')
        if not os.path.exists(self.artifacts_unarchived):
            raise MissingDirectoryException(dirname=self.artifacts_unarchived)
        if not os.path.exists(self.gg_root):
            raise ValidationException(f'No greengrass installation found in {self.gg_root}, '
                                      f'please install greengrass and try again')

    def deploy(self):
        self._validate()

        print(f'Stopping greengrass')
        stop_greengrass()

        dst_artifact = os.path.join(self.deploy_dir, 'emqx.zip')

        print(f'Copying build artifact to {dst_artifact}')
        shutil.copy2(src=EMQX_ZIP, dst=dst_artifact)

        print(f'Removing existing deployment')
        shutil.rmtree(path=os.path.join(self.deploy_dir, 'emqx'), ignore_errors=True)

        print(f'Extracting build artifact')
        with zipfile.ZipFile(dst_artifact, 'r') as z:
            z.extractall(os.path.join(self.deploy_dir, 'emqx'))
        os.remove(dst_artifact)

        print(f'Starting greengrass')
        start_greengrass()

        print(f'Deployment complete')


if __name__ == '__main__':
    if os.name != 'nt':
        raise NotImplementedError('unix/docker not yet supported')

    parser = argparse.ArgumentParser(description='Deploy emqx component locally to greengrass core device. '
                                                 'The latest component version in artifacts-unarchived will be '
                                                 'replaced with contents of emqx.zip. NOTE: This script will only work'
                                                 'after a normal cloud deployment of EMQX component has been performed'
                                                 'on this device at least once.')
    parser.add_argument('--root',
                        help='Path to Greengrass v2 root dir',
                        required=False,
                        default=DEFAULT_GREENGRASS_ROOT)
    args = vars(parser.parse_args())
    root = os.path.abspath(args['root'])

    deployer = LocalDeployer(gg_root=root)
    deployer.deploy()
