## Greengrass EMQX Authentication/Authorization Plugin

This repository hosts a plugin and build system for Greengrass's aws.greengrass.clientdevices.mqtt.EMQX component.

## Building

To build EMQX with the Greengrass authentication and authorization plugin, install:

- python3
- cmake
- Erlang/OTP
- Native buildtools (gcc/clang/msvc)

See Dockerfile for a full list of requirements (for Alpine Linux at least).

Run `python3 -u -m bin` in order to build everything. The built EMQX will be in `/build` as `emqx.zip`.

### Build Options

`python3 -u -m bin` has several options when building. The default (no options) will build everything and execute tests.

- Use `--no-test` to skip testing.
- Use `--quick` to build only the EMQX auth plugin. Must perform a full build first.
- Use `--sdk-only` to build only the AWS IoT Device SDK for C++.
- Use `--test-only` to only rebuild and test the EMQX auth plugin. Must perform a full build first.

## Security

See [CONTRIBUTING](CONTRIBUTING.md#security-issue-notifications) for more information.

## License

This project is licensed under the Apache-2.0 License.

## Distribution

This software includes code distributed under the Microsoft Software License Terms - Microsoft Visual Studio
Community 2022. By downloading this software, you agree to that code's license terms, which can be found
[here](https://visualstudio.microsoft.com/license-terms/vs2022-ga-community)
