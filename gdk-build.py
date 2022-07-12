import shutil
import os.path

COMPONENT_NAME = 'aws.greengrass.clientdevices.mqtt.EMQX'
COMPONENT_VERSION = 'NEXT_PATCH'

if __name__ == '__main__':
    if bool(os.getenv('BUILD')):
        import bin
        bin.main()

    shutil.copy(
        os.path.join('recipe.json'),
        os.path.join('greengrass-build', 'recipes'))

    shutil.copy(
        os.path.join('build', 'emqx.zip'),
        os.path.join('greengrass-build', 'artifacts', COMPONENT_NAME, COMPONENT_VERSION))
