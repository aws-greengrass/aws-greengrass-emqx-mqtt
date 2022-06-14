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
    libunwind-dev \
    linux-headers \
    lksctp-tools-dev \
    make \
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

COPY . /build

RUN cd /build \
    && python3 -u -m pip install -r bin/requirements.txt \
    && python3 -u -m bin \
    && cd build \
    && unzip -q emqx.zip

FROM ${RUN_FROM}

COPY --from=builder /build/emqx/deploy/docker/docker-entrypoint.sh /usr/bin/
COPY --from=builder /build/build/emqx/ /opt/emqx

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
ENV EMQX_LISTENER__SSL__EXTERNAL__KEYFILE="/opt/emqx/data/key.pem"
ENV EMQX_LISTENER__SSL__EXTERNAL__CERTFILE="/opt/emqx/data/cert.pem"
ENV EMQX_LISTENER__SSL__EXTERNAL__CACERTFILE="/opt/emqx/data/cacert.pem"

# emqx will occupy these port:
# - 8883 port for MQTT(SSL)
EXPOSE 8883

ENTRYPOINT ["/usr/bin/docker-entrypoint.sh"]

CMD ["/opt/emqx/bin/emqx", "foreground"]
