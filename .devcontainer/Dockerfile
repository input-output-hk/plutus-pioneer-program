ARG UBUNTU_VERSION=20.04
FROM ubuntu:${UBUNTU_VERSION} as firstStage
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
  && ghcup set ghc
RUN ghcup install hls ${HLS_VERSION}

# install nix and niv
RUN curl -L -o install.sh https://nixos.org/nix/install \
  && bash install.sh --daemon
  
FROM firsStage as secondStage
RUN source ~/.bashrc
RUN nix-env -iA nixpkgs.niv
