ARG RUN_FROM=public.ecr.aws/docker/library/alpine:3.15.4
FROM ${RUN_FROM} AS builder

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


COPY .git /build/.git
COPY .gitmodules /build/.gitmodules
COPY bin /build/bin

# Build AWS IoT SDK
RUN cd /build \
    && python3 -u -m pip install -r bin/requirements.txt \
    && python3 -u -m bin --sdk-only

# Build EMQX
COPY emqx.commit /build/
RUN cd /build \
    && python3 -u -m bin --emqx-only

# Full build, including port driver, greengrass auth EMQX plugin, utilizing above cached layers
COPY THIRD-PARTY-LICENSES emqx.commit /build/
COPY patches /build/patches
COPY port_driver /build/port_driver
COPY aws_greengrass_emqx_auth /build/aws_greengrass_emqx_auth
RUN cd /build \
    && python3 -u -m bin --no-test \
    && cd build \
    && unzip -q emqx.zip

FROM ${RUN_FROM}

COPY --from=builder /build/patches/entrypoint.sh /usr/bin/
COPY --from=builder /build/emqx/deploy/docker/docker-entrypoint.sh /usr/bin/
COPY --from=builder /build/build/emqx/ /opt/emqx

# Remove the default certs that ship with EMQX just to be sure they can't be used for any reason
RUN rm -rf /opt/emqx/etc/certs
# Backup the etc files so that we always have the defaults available to us
RUN cp -r /opt/emqx/etc /opt/emqx/etcOrig
RUN cp -r /opt/emqx/data /opt/emqx/dataOrig
RUN ln -s /opt/emqx/bin/* /usr/local/bin/
RUN apk add --no-cache curl ncurses-libs openssl libstdc++ bash

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
ENV EMQX_LISTENER__SSL__EXTERNAL__KEYFILE="/opt/emqx/data/key.pem"
ENV EMQX_LISTENER__SSL__EXTERNAL__CERTFILE="/opt/emqx/data/cert.pem"

# emqx will occupy these port:
# - 8883 port for MQTT(SSL)
EXPOSE 8883

ENTRYPOINT ["/usr/bin/entrypoint.sh"]

CMD ["/usr/bin/docker-entrypoint.sh", "/opt/emqx/bin/emqx", "foreground"]
