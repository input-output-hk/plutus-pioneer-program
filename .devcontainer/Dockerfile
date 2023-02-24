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

# install nix
# RUN curl -L -o install.sh https://nixos.org/nix/install \
#   && bash install.sh --daemon

FROM system_deps as haskell

#SHELL ["bash", "-lc"]

# haskell args
ARG CABAL_VERSION=3.6.2.0
ARG GHC_VERSION=8.10.7
ARG HLS_VERSION=1.7.0.0


#RUN nix-env -iA niv -f https://github.com/nmattia/niv/tarball/master --substituters https://niv.cachix.org --trusted-public-keys niv.cachix.org-1:X32PCg2e/zAm3/uD1ScqW2z/K0LtDyNV7RdaxIuLgQM=
#  && niv add mlabs-haskell/plutus-simple-model -r 64dd3fee4caff9472e0dcc2e14cc587452c80e77

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

RUN ghcup config set downloader Wget \
  &&  ghcup install ghc ${GHC_VERSION} \
  &&  ghcup install cabal ${CABAL_VERSION}
RUN ghcup set ghc ${GHC_VERSION}
RUN ghcup install hls ${HLS_VERSION}

ENV PATH=${PATH}:${HOME:-/root}/.cabal/bin
#RUN echo "export PATH=$PATH:/root/.cabal/bin" >> ~/.bashrc
#RUN cabal update

FROM haskell as cardano_node

# install cardano-node binaries
RUN wget https://update-cardano-mainnet.iohk.io/cardano-node-releases/cardano-node-1.35.5-linux.tar.gz \
  && tar -xf cardano-node-1.35.5-linux.tar.gz -C /usr/bin \
  && cardano-cli --version

# node socket
RUN echo "export CARDANO_NODE_SOCKET_PATH=/ipc/node.socket" >> ~/.bashrc
RUN mkdir -p ${HOME:-/root}/node-config

# Download cardano-node config
RUN mkdir -p /ipc

RUN  wget https://book.world.dev.cardano.org/environments/preview/config.json \
  && wget https://book.world.dev.cardano.org/environments/preview/db-sync-config.json \
  && wget https://book.world.dev.cardano.org/environments/preview/submit-api-config.json \
  && wget https://book.world.dev.cardano.org/environments/preview/topology.json \
  && wget https://book.world.dev.cardano.org/environments/preview/byron-genesis.json \
  && wget https://book.world.dev.cardano.org/environments/preview/alonzo-genesis.json \
  && wget https://book.world.dev.cardano.org/environments/preview/shelley-genesis.json \
  && mv ./*.json ${HOME:-/root}/node-config/

# start-node alias
RUN echo "alias start-node='cardano-node run \
	     --config ${HOME:-/root}/node-config/config.json \
	     --topology ${HOME:-/root}/node-config/topology.json \
	     --database-path ${HOME:-/root}/preview-db \
	     --socket-path /ipc/node.socket'" >> ~/.bashrc

FROM cardano_node as kuber

# In case we need to build from source
# install kuber
#RUN git clone https://github.com/dQuadrant/kuber \
#  && cd kuber \
#  && cabal update \
#  && cd server \
#  && cabal build \
#  && cabal install

# install kuber
RUN wget https://github.com/rober-m/kuber/releases/download/babbage-era/$(arch)-linux-kuber \
  && chmod +x $(arch)-linux-kuber \
  && mv $(arch)-linux-kuber /usr/bin/kuber \
  && echo "export NETWORK=preview" >> ~/.bashrc


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

RUN git clone https://github.com/input-output-hk/plutus-pioneer-program \
  && cd plutus-pioneer-program/code \
  && cabal update \
  && cabal build all \
  && cabal install stylish-haskell

RUN echo "alias serve-docs='python3 -m http.server -d /workspaces/plutus-pioneer-program/docs/plutus-docs'" >> ~/.bashrc