ARG UBUNTU_VERSION=20.04
FROM ubuntu:${UBUNTU_VERSION} as system_deps
ENV DEBIAN_FRONTEND=nonintercative
RUN mkdir -p /app/src
WORKDIR /app

# system_deps args
ARG IOHK_LIBSODIUM_GIT_REV=66f017f16633f2060db25e17c170c2afa0f2a8a1
ARG IOKH_LIBSECP251_GIT_REV=ac83be33d0956faf6b7f61a60ab524ef7d6a473a

# development dependencies
RUN apt-get update -y && apt-get install -y \
  curl \
  xz-utils \
  automake \
  build-essential \
  g++\
  git \
  jq \
  libicu-dev \
  libffi-dev \
  libgmp-dev \
  libncursesw5 \
  libpq-dev \
  libssl-dev \
  libsystemd-dev \
  libtinfo-dev \
  libtool \
  make \
  pkg-config \
  tmux \
  wget \
  zlib1g-dev libreadline-dev llvm libnuma-dev \
  && rm -rf /var/lib/apt/lists/*

# install secp2561k library with prefix '/'
RUN git clone https://github.com/bitcoin-core/secp256k1 &&\
  cd secp256k1 \
  && git fetch --all --tags &&\
  git checkout ${IOKH_LIBSECP251_GIT_REV} \
  && ./autogen.sh && \
  ./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental && \
  make && \
  make install  && cd .. && rm -rf ./secp256k1

FROM system_deps as haskell

# haskell args
ARG CABAL_VERSION=3.6.2.0
ARG GHC_VERSION=8.10.7
ARG HLS_VERSION=1.7.0.0

# install libsodium from sources with prefix '/'
RUN git clone https://github.com/input-output-hk/libsodium.git &&\
  cd libsodium \
  && git fetch --all --tags &&\
  git checkout ${IOHK_LIBSODIUM_GIT_REV} \
  && ./autogen.sh && \
  ./configure --prefix=/usr && \
  make && \
  make install  && cd .. && rm -rf ./libsodium

# install ghcup
ENV PATH=${PATH}:${HOME:-/root}/.ghcup/bin
RUN wget --secure-protocol=TLSv1_2 \
  https://downloads.haskell.org/~ghcup/$(arch)-linux-ghcup  \ 
  && chmod +x $(arch)-linux-ghcup \
  && mkdir -p ${HOME:-/root}/.ghcup/bin \
  && mv $(arch)-linux-ghcup ${HOME:-/root}/.ghcup/bin/ghcup 

# install ghc, caball, and hls
RUN ghcup config set downloader Wget \
  &&  ghcup install ghc ${GHC_VERSION} \
  &&  ghcup install cabal ${CABAL_VERSION}
RUN ghcup set ghc ${GHC_VERSION}
RUN ghcup install hls ${HLS_VERSION}

# Update cabal
RUN cabal update

# Add cabal to PATH
RUN echo "export PATH=$PATH:/root/.cabal/bin" >> ~/.bashrc

FROM haskell as cardano_cli

# install cardano-cli (we use a different docker container for the actual node)
# (These have to be the same version as the cardano-node image specified at `docker-compose.yml`)
RUN wget https://github.com/rober-m/cardano-node/releases/download/1.35.5/$(arch)-linux-cardano-cli
RUN chmod +x $(arch)-linux-cardano-cli
RUN mv $(arch)-linux-cardano-cli /usr/bin/cardano-cli

# node socket
RUN echo "export CARDANO_NODE_SOCKET_PATH=/root/.cardano/preview/node.socket" >> ~/.bashrc
RUN echo "export NETWORK=preview" >> ~/.bashrc

FROM cardano_cli as kuber

# install kuber
RUN wget https://github.com/rober-m/kuber/releases/download/babbage-era/$(arch)-linux-kuber
RUN chmod +x $(arch)-linux-kuber
RUN mv $(arch)-linux-kuber /usr/bin/kuber


FROM kuber as stylish_haskell

# install stylish-haskell (FIXME: couldn't compile a bin for arm64)
# RUN wget https://github.com/rober-m/stylish-haskell/releases/download/v0.14.3.0/$(arch)-linux-stylish-haskell
# RUN chmod +x $(arch)-linux-stylish-haskell
# RUN mv $(arch)-linux-stylish-haskell /usr/bin/stylish-haskell
RUN wget https://github.com/rober-m/stylish-haskell/releases/download/v0.14.3.0/x86_64-linux-stylish-haskell
RUN chmod +x x86_64-linux-stylish-haskell
RUN mv x86_64-linux-stylish-haskell /usr/bin/stylish-haskell

FROM kuber as nodejs

ENV NODE_VERSION=16.13.0
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
ENV NVM_DIR=/root/.nvm
RUN . "$NVM_DIR/nvm.sh" && nvm install ${NODE_VERSION}
RUN . "$NVM_DIR/nvm.sh" && nvm use v${NODE_VERSION}
RUN . "$NVM_DIR/nvm.sh" && nvm alias default v${NODE_VERSION}
ENV PATH="/root/.nvm/versions/node/v${NODE_VERSION}/bin/:${PATH}"
RUN node --version
RUN npm --version

FROM nodejs as plutus

RUN git clone https://github.com/input-output-hk/plutus-pioneer-program
RUN cd plutus-pioneer-program/code && cabal update
RUN cd plutus-pioneer-program/code && cabal build all

RUN echo "alias serve-docs='python3 -m http.server -d /workspace/docs/plutus-docs/haddock/'" >> ~/.bashrc
RUN echo "source <(cardano-cli --bash-completion-script cardano-cli)" >> ~/.bashrc

FROM plutus as cargo

RUN curl https://sh.rustup.rs -sSfo rustup-init
RUN chmod +x rustup-init
RUN ./rustup-init -y

FROM cargo as deno

RUN /root/.cargo/bin/cargo install deno --locked

FROM deno as testing-docs

RUN /root/.cargo/bin/cargo install mdbook
RUN echo "alias serve-testing-docs='mdbook serve /workspace/docs/psm-docs'" >> ~/.bashrc