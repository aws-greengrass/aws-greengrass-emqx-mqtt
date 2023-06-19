import shutil
import os.path
import subprocess

FORCE = bool(os.getenv('FORCE'))
DOCKER_ONLY = bool(os.getenv('DOCKER_ONLY'))

COMPONENT_NAME = 'aws.greengrass.clientdevices.mqtt.EMQX'
COMPONENT_VERSION = 'NEXT_PATCH'

RECIPES_DIR = os.path.join('greengrass-build', 'recipes')
ARTIFACTS_DIR = os.path.join('greengrass-build', 'artifacts', COMPONENT_NAME, COMPONENT_VERSION)

ZIP_ARTIFACT = os.path.join('build', 'emqx.zip')

TAG = 'aws-greengrass-emqx:amd64'
IMAGE_FILE = os.path.join(ARTIFACTS_DIR, f'{TAG}.amd64.tar.gz')


def build_zip() -> None:
    if not os.path.exists(ZIP_ARTIFACT) or FORCE:
        import bin
        bin.main()
    shutil.copy(ZIP_ARTIFACT, ARTIFACTS_DIR)


def build_docker() -> None:
    if not os.path.exists(IMAGE_FILE) or FORCE:
        subprocess.check_call(f'docker build . -t {TAG}', shell=True)
        # TODO make more system independent
        subprocess.check_call(f'docker save {TAG} | gzip > {IMAGE_FILE}', shell=True)


if __name__ == '__main__':
    if not DOCKER_ONLY:
        build_zip()
    build_docker()
    shutil.copy('recipe.json', RECIPES_DIR)
