ARG UBUNTU_VERSION=20.04
FROM ubuntu:${UBUNTU_VERSION} as first_stage
ENV DEBIAN_FRONTEND=nonintercative
RUN mkdir -p /app/src
WORKDIR /app

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

ARG CABAL_VERSION=3.6.2.0
ARG GHC_VERSION=8.10.7
ARG HLS_VERSION=1.7.0.0
ARG IOHK_LIBSODIUM_GIT_REV=66f017f16633f2060db25e17c170c2afa0f2a8a1
ARG IOKH_LIBSECP251_GIT_REV=ac83be33d0956faf6b7f61a60ab524ef7d6a473a

# install secp2561k library with prefix '/'
RUN git clone https://github.com/bitcoin-core/secp256k1 &&\
  cd secp256k1 \
  && git fetch --all --tags &&\
  git checkout ${IOKH_LIBSECP251_GIT_REV} \
  && ./autogen.sh && \
  ./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental && \
  make && \
  make install  && cd .. && rm -rf ./secp256k1


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
  &&  ghcup install cabal ${CABAL_VERSION} \
  && ghcup set ghc ${GHC_VERSION}
RUN ghcup install hls ${HLS_VERSION}

# install nix and niv
RUN curl -L -o install.sh https://nixos.org/nix/install \
  && bash install.sh --daemon
  
FROM first_stage as second_stage
SHELL ["bash", "-lc"]
#RUN nix-env -iA niv -f https://github.com/nmattia/niv/tarball/master --substituters https://niv.cachix.org --trusted-public-keys niv.cachix.org-1:X32PCg2e/zAm3/uD1ScqW2z/K0LtDyNV7RdaxIuLgQM=
#  && niv add mlabs-haskell/plutus-simple-model -r 64dd3fee4caff9472e0dcc2e14cc587452c80e77

# install cardano-node binaries
RUN wget https://update-cardano-mainnet.iohk.io/cardano-node-releases/cardano-node-1.35.5-linux.tar.gz \
  && tar -xf cardano-node-1.35.5-linux.tar.gz -C /usr/bin \
  && cardano-cli --version

RUN echo "export CARDANO_NODE_SOCKET_PATH=/ipc/node.socket" >> ~/.bashrc

# install kuber
RUN git clone https://github.com/dQuadrant/kuber \
  && cd kuber \
  && cabal update \
  && cd server \
  && cabal install

RUN echo "export PATH=$PATH:/root/.cabal/bin/kuber" >> ~/.bashrc