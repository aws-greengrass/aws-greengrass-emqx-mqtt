# Development

## Windows

It's recommended to use a single Windows Server EC2 machine,
for both development and as a core device for testing.

Tips for reducing development iteration time:
* Use the `--quick` flag when building
* Deployments can be done by copying the build artifact, `build/emqx.zip`, to `\greengrass\v2\packages\artifacts\aws.greengrass.clientdevices.mqtt.EMQX\{version}\` 
and restarting EMQX via greengrass cli. 
This is faster than creating a new greengrass component version and updating a deployment.
