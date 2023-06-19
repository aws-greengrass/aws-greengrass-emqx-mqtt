import shutil
import os.path
import subprocess

FORCE = bool(os.getenv('FORCE'))

COMPONENT_NAME = 'aws.greengrass.clientdevices.mqtt.EMQX'
COMPONENT_VERSION = 'NEXT_PATCH'

RECIPES_DIR = os.path.join('greengrass-build', 'recipes')
ARTIFACTS_DIR = os.path.join('greengrass-build', 'artifacts', COMPONENT_NAME, COMPONENT_VERSION)


def build_zip() -> None:
    file = os.path.join('build', 'emqx.zip')
    artifact = os.path.join(ARTIFACTS_DIR, file)
    if not os.path.exists(artifact) or FORCE:
        import bin
        bin.main()
        shutil.copy(file, ARTIFACTS_DIR)


def build_docker() -> None:
    tag = 'aws-greengrass-emqx:amd64'
    artifact = os.path.join(ARTIFACTS_DIR, 'aws-greengrass-emqx.amd64.tar.gz')
    if not os.path.exists(artifact) or FORCE:
        subprocess.check_call(f'docker build . -t {tag}', shell=True)
        # TODO make more system independent
        subprocess.check_call(f'docker save {tag} | gzip > {artifact}', shell=True)


if __name__ == '__main__':
    if not DOCKER_ONLY:
        build_zip()
    build_docker()
    shutil.copy('recipe.json', RECIPES_DIR)
