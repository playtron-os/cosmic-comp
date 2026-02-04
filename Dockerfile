ARG version=1.90
FROM rust:${version}
ARG version

RUN dpkg --add-architecture arm64
RUN apt-get update && apt-get install -y \
  cmake \
  libclang-dev \
  libudev-dev \
  libudev-dev:arm64 \
  libseat-dev \
  libseat-dev:arm64 \
  libdisplay-info-dev \
  libdisplay-info-dev:arm64 \
  libinput-dev \
  libinput-dev:arm64 \
  libxkbcommon-dev \
  libxkbcommon-dev:arm64 \
  libgbm-dev \
  libgbm-dev:arm64 \
  libpixman-1-dev \
  libpixman-1-dev:arm64 \
  libssl-dev:arm64

RUN apt-get install -y \
  g++-aarch64-linux-gnu \
  libc6-dev-arm64-cross

# Taskfile support
RUN curl -1sLf 'https://dl.cloudsmith.io/public/task/task/setup.deb.sh' | bash
RUN apt-get install -y task

# RPM support
RUN apt-get install -y rpm librpmbuild10 elfutils

RUN rustup target add --toolchain $version aarch64-unknown-linux-gnu
RUN rustup toolchain install --force-non-host $version-aarch64-unknown-linux-gnu
RUN rustup component add clippy
RUN chmod -R 777 /usr/local/rustup

ENV CARGO_TARGET_AARCH64_UNKNOWN_LINUX_GNU_LINKER=aarch64-linux-gnu-gcc
ENV CC_aarch64_unknown_linux_gnu=aarch64-linux-gnu-gcc
ENV CXX_aarch64_unknown_linux_gnu=aarch64-linux-gnu-g++

