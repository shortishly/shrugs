ARG BUILD_IMAGE
FROM ${BUILD_IMAGE}

ARG RUST_VERSION

RUN apt-get update
RUN apt-get install -y emacs

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- --default-toolchain ${RUST_VERSION} -y
ENV PATH="$PATH:/root/.cargo/bin"
