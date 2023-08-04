#!/usr/bin/env python

import argparse
# semver without need for external dependency
import shutil

import packaging.version
import json
import os
import subprocess
import sys
import time

EMQX_COMPONENT = 'aws.greengrass.clientdevices.mqtt.EMQX'

SCRIPT_DIR = sys.path[0]
DEFAULT_GREENGRASS_ROOT = os.path.join(os.sep, 'greengrass', 'v2')
DEFAULT_RECIPE_DIR = os.path.join(SCRIPT_DIR, '..',  'greengrass-build', 'recipes')

CONFIG_UPDATE_A = {
    EMQX_COMPONENT: {
        'RESET': [''],
        'MERGE': {
            'emqx': {
                'log.level': 'debug'
            },
            'crtLogLevel': 'debug'
        },
    }
}

CONFIG_UPDATE_B = {
    EMQX_COMPONENT: {
        'RESET': [''],
        'MERGE': {
            'emqx': {
                'log.level': 'debug'
            },
            'crtLogLevel': 'debug',
            'emqxConfig': {
                'dashboard': {
                    'listeners': {
                        'http': {
                            'bind': 18083
                        }
                    }
                }
            }
        }
    }
}


class LocalDeployer:

    def __init__(self, root, recipe_dir):
        self.root = root
        self.recipe_dir = recipe_dir

    def create_deployment(self, config_update):
        resp = subprocess.check_output([
            'greengrass-cli.cmd', 'deployment', 'create',
            '--merge', f'{EMQX_COMPONENT}={self.find_latest_component_version()}',
            '--recipeDir', self.recipe_dir,
            '--update-config', json.dumps(config_update)])
        deployment_id = resp.strip().split()[-1].decode('utf-8')
        print(f'deployment {deployment_id} created')
        return deployment_id

    def wait_for_deployment(self, deployment_id):
        for i in range(10):
            if i == 0:
                print('waiting for deployment to complete...')
            try:
                resp = subprocess.check_output([
                    'greengrass-cli.cmd', 'deployment', 'status',
                    '-i', deployment_id
                ])
            except subprocess.CalledProcessError:
                continue
            deployment_status = resp.strip().split()[-1].decode('utf-8')
            print(deployment_status)
            if deployment_status == 'FAILED':
                raise ValueError(f'Local deployment failed, check '
                                 f"{os.path.join(self.root, 'logs', 'greengrass.log')} or "
                                 f"{os.path.join(self.root, 'logs', f'{EMQX_COMPONENT}.log')} "
                                 f"for errors")
            if deployment_status == 'SUCCEEDED':
                return
            time.sleep(5)
        raise ValueError('Local deployment did not complete in time')

    def find_latest_component_version(self):
        artifacts_unarchived = os.path.join(
            self.root, 'packages', 'artifacts-unarchived', EMQX_COMPONENT)

        def parse_semver(path: os.PathLike) -> packaging.version.Version:
            return packaging.version.parse(os.path.basename(path))

        unarchived_versions = next(os.walk(artifacts_unarchived))[1]
        if not unarchived_versions:
            raise ValueError('Unable to find latest component version')
        return str(max(unarchived_versions, key=parse_semver))


class ConfigChanger:

    def __init__(self, deployer, iterations=500):
        self.deployer = deployer
        self.iterations = iterations

    def start(self):
        for _ in range(self.iterations):
            self._apply_config_update(CONFIG_UPDATE_A)
            self._apply_config_update(CONFIG_UPDATE_B)

    def _apply_config_update(self, config_update):
        deployment_id = self.deployer.create_deployment(config_update=config_update)
        self.deployer.wait_for_deployment(deployment_id=deployment_id)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Send configuration updates to EMQX component via a local deployment.')
    parser.add_argument('--root',
                        help='Path to Greengrass v2 root dir',
                        required=False,
                        default=DEFAULT_GREENGRASS_ROOT)
    parser.add_argument('--recipe-dir',
                        help='Path to recipe dir',
                        required=False,
                        default=DEFAULT_RECIPE_DIR)
    args = vars(parser.parse_args())
    _root = os.path.abspath(args['root'])

    _recipe_dir = os.path.abspath(args['recipe_dir'])
    if not os.path.exists(_recipe_dir):
        if args['recipe_dir'] == DEFAULT_RECIPE_DIR:
            raise ValueError(f'GDK recipe dir, {_recipe_dir}, not found, please run "gdk component build" first')
        raise ValueError(f'Recipe dir {_recipe_dir} does not exist')

    _local_deployer = LocalDeployer(root=_root, recipe_dir=_recipe_dir)
    cc = ConfigChanger(deployer=_local_deployer)

    greengrass_build_dir = os.path.join(SCRIPT_DIR, '..', 'greengrass-build')
    if args['recipe_dir'] == DEFAULT_RECIPE_DIR and os.path.exists(greengrass_build_dir):
        print(f'Recreating {greengrass_build_dir} to start with clean slate')
        shutil.rmtree(path=greengrass_build_dir, ignore_errors=True)
        subprocess.check_call(['gdk', 'component', 'build'])
        print(f'Injecting latest local version into recipe file')
        recipe_file = os.path.join(greengrass_build_dir, 'recipes', 'recipe.json')
        with open(recipe_file, 'r') as f:
            recipe = json.loads(f.read())
        recipe['ComponentVersion'] = _local_deployer.find_latest_component_version()
        with open(recipe_file, 'w') as f:
            f.write(json.dumps(recipe))

    # run in bin dir in case gg cli isn't on path
    os.chdir(os.path.join(_root, 'bin'))
    cc.start()
