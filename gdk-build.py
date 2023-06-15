import shutil
import os.path
import subprocess

COMPONENT_NAME = 'aws.greengrass.clientdevices.mqtt.EMQX'
COMPONENT_VERSION = 'NEXT_PATCH'

TAG = 'aws-greengrass-emqx'
IMAGE = f'{TAG}.amd64.tar.gz'

RECIPES_DIR = os.path.join('greengrass-build', 'recipes')
ARTIFACTS_DIR = os.path.join('greengrass-build', 'artifacts', COMPONENT_NAME, COMPONENT_VERSION)

if __name__ == '__main__':
    skip_cache = bool(os.getenv('SKIP_CACHE'))
    docker = bool(os.getenv('DOCKER'))

    if not docker and (not os.path.exists('emqx/emqx.zip') or skip_cache):
        import bin
        bin.main()

    if docker and (not os.path.exists(IMAGE) or skip_cache):
        subprocess.check_call(f'docker build{" --no-cache" if skip_cache else ""} . -t {TAG}', shell=True)
        # TODO make more system independent
        subprocess.check_call(f'docker save {TAG} | gzip > {IMAGE}', shell=True)

    recipe_file = 'recipe-docker.json' if docker else 'recipe.json'
    artifact = IMAGE if docker else os.path.join('build', 'emqx.zip')

    shutil.copy(recipe_file, RECIPES_DIR)
    shutil.copy(artifact, ARTIFACTS_DIR)
