ARG RUN_FROM=public.ecr.aws/docker/library/alpine:3.15.4
FROM ${RUN_FROM} AS build-base

RUN apk add --no-cache \
    autoconf \
    automake \
    bash \
    bsd-compat-headers \
    ca-certificates \
    cmake \
    coreutils \
    curl \
    dpkg-dev dpkg \
    erlang-dev \
    g++ \
    gcc \
    git \
    jq \
    libc-dev \
    libffi-dev \
    libtool \
    libunwind-dev \
    linux-headers \
    lksctp-tools-dev \
    make \
    maven \
    ncurses-dev \
    openjdk8 \
    openssh-client \
    openssh-keygen \
    openssl-dev \
    py3-pip \
    python3 \
    sudo \
    tar \
    unixodbc-dev \
    wget \
    zip \
    zlib-dev

WORKDIR /build

COPY bin/requirements.txt bin/requirements.txt
RUN python3 -u -m pip install -r bin/requirements.txt

# Copy common build files.
# Component-specific build files will be copied in subsequent layers,
# This lets us modify the build process for specific components
# without invalidating layers of the build cache unnecessarily.
COPY bin/__init__.py bin/__main__.py bin/build.py bin/utils.py bin/

FROM build-base as sdk

WORKDIR /build

COPY .gitmodules .gitmodules
# TODO this breaks layer caching
COPY .git .git

COPY --from=build-base /build/bin bin
COPY bin/build_sdk.py bin

RUN python3 -u -m bin --sdk-only

FROM build-base as emqx

WORKDIR /build

COPY --from=build-base /build/bin bin
COPY emqx.commit .
COPY bin/build_emqx.py bin
RUN python3 -u -m bin --emqx-only

FROM build-base as port-driver

WORKDIR /build

COPY --from=build-base /build/bin bin
COPY --from=sdk /build/_build_sdk _build_sdk
COPY port_driver port_driver
COPY bin/build_port_driver.py bin
RUN python3 -u -m bin --port-driver-only --no-test

FROM build-base as plugin

WORKDIR /build

COPY --from=build-base /build/bin bin
COPY --from=emqx /build/emqx/rebar3 emqx/rebar3
COPY --from=port-driver /build/_build/driver_lib _build/driver_lib
COPY patches/msvcp140.dll patches/vcruntime140.dll patches/vcruntime140_1.dll patches/
COPY aws_greengrass_emqx_auth aws_greengrass_emqx_auth
COPY bin/build_plugin.py bin/package.py bin/
COPY .git .git
RUN python3 -u -m bin --plugin-only

FROM build-base as package

WORKDIR /build

COPY --from=build-base /build/bin bin
COPY --from=emqx /build/emqx/ emqx
COPY --from=plugin /build/emqx/_build/emqx/rel/emqx/plugins/aws_greengrass_emqx_auth-1.0.0 emqx/_build/emqx/rel/emqx/plugins/aws_greengrass_emqx_auth-1.0.0
COPY THIRD-PARTY-LICENSES .
COPY patches/emqx.cmd.diff patches/emqx.conf patches/emqx.diff patches/msvcr120.dll patches/acl.conf patches/
COPY bin/package.py bin/build_emqx.py bin/
RUN python3 -u -m bin --package-only \
    && cd build \
    && unzip -q emqx.zip


FROM ${RUN_FROM}

RUN apk add --no-cache curl ncurses-libs openssl libstdc++ bash

COPY patches/entrypoint.sh /usr/bin/
COPY --from=emqx /build/emqx/deploy/docker/docker-entrypoint.sh /usr/bin/
COPY --from=package /build/build/emqx/ /opt/emqx

# Remove the default certs that ship with EMQX just to be sure they can't be used for any reason
RUN rm -rf /opt/emqx/etc/certs
# Backup the etc files so that we always have the defaults available to us
RUN cp -r /opt/emqx/etc /opt/emqx/etcOrig
RUN cp -r /opt/emqx/data /opt/emqx/dataOrig
RUN ln -s /opt/emqx/bin/* /usr/local/bin/

WORKDIR /opt/emqx

RUN adduser -D -u 1000 emqx
RUN chgrp -Rf emqx /opt/emqx && chmod -Rf a+x /opt/emqx \
    && chown -Rf emqx /opt/emqx

USER emqx

VOLUME ["/opt/emqx/log", "/opt/emqx/data"]

# Set env variables for docker container
ENV EMQX_LOG__DIR="/opt/emqx/log"
ENV EMQX_NODE__DATA_DIR="/opt/emqx/data"
ENV ORIG_EMQX_NODE__DATA_DIR="/opt/emqx/dataOrig"
ENV EMQX_NODE__ETC_DIR="/opt/emqx/etc"
ENV ORIG_EMQX_NODE__ETC_DIR="/opt/emqx/etcOrig"
ENV EMQX_LISTENERS__SSL__MTLS__SSL_OPTIONS__KEYFILE="/opt/emqx/data/key.pem"
ENV EMQX_LISTENERS__SSL__MTLS__SSL_OPTIONS__CERTFILE="/opt/emqx/data/cert.pem"

# emqx will occupy these port:
# - 8883 port for MQTT(SSL)
EXPOSE 8883

ENTRYPOINT ["/usr/bin/entrypoint.sh"]

CMD ["/usr/bin/docker-entrypoint.sh", "/opt/emqx/bin/emqx", "foreground"]
