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
    tar \
    unixodbc-dev \
    wget \
    zip \
    zlib-dev

COPY . /build

ARG EMQX_NAME=emqx
ENV EMQX_RELUP=false

RUN cd /build \
    && export PROFILE="$EMQX_NAME" \
    && export EMQX_NAME=${EMQX_NAME%%-elixir} \
    && export EMQX_LIB_PATH="emqx/_build/$EMQX_NAME/lib" \
    && export EMQX_REL_PATH="/build/emqx/_build/$EMQX_NAME/rel/emqx" \
    && export EMQX_REL_FORM='docker' \
    && cd emqx \
    && rm -rf $EMQX_LIB_PATH \
    && make $PROFILE \
    && mkdir -p /emqx-rel \
    && mv $EMQX_REL_PATH /emqx-rel \
    && cd /build/sdk/build \
    && cmake -DCMAKE_INSTALL_PREFIX="/build/sdk" ../aws-iot-device-sdk-cpp-v2 \
    && cmake --build .

FROM ${RUN_FROM}

COPY emqx/deploy/docker/docker-entrypoint.sh /usr/bin/
COPY --from=builder /emqx-rel/emqx /opt/emqx

RUN ln -s /opt/emqx/bin/* /usr/local/bin/
RUN apk add --no-cache curl ncurses-libs openssl sudo libstdc++ bash

WORKDIR /opt/emqx

RUN adduser -D -u 1000 emqx

RUN chgrp -Rf emqx /opt/emqx && chmod -Rf g+w /opt/emqx \
    && chown -Rf emqx /opt/emqx

USER emqx

VOLUME ["/opt/emqx/log", "/opt/emqx/data"]

# emqx will occupy these port:
# - 8883 port for MQTT(SSL)
EXPOSE 8883

ENTRYPOINT ["/usr/bin/docker-entrypoint.sh"]

CMD ["/opt/emqx/bin/emqx", "foreground"]
