ARG UBUNTU_VERSION
FROM ubuntu:${UBUNTU_VERSION} AS system_deps
ENV DEBIAN_FRONTEND=nonintercative
RUN mkdir -p /app/src
WORKDIR /app

# system_deps args
ARG IOHK_LIBSODIUM
ARG IOHK_LIBSODIUM_GIT_REV
ARG BTC_LIBSECP251
ARG BTC_LIBSECP251_GIT_REV

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
  cmake \
  && rm -rf /var/lib/apt/lists/*

# install secp2561k library with prefix '/'
RUN git clone ${BTC_LIBSECP251} &&\
  cd secp256k1 \
  && git fetch --all --tags &&\
  git checkout ${BTC_LIBSECP251_GIT_REV} \
  && ./autogen.sh && \
  ./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental && \
  make && \
  make install  && cd .. && rm -rf ./secp256k1

FROM system_deps AS haskell

# haskell args
ARG CABAL_VERSION
ARG GHC_VERSION
ARG HLS_VERSION

# install libsodium from sources with prefix '/'
RUN git clone ${IOHK_LIBSODIUM} &&\
  cd libsodium \
  && git fetch --all --tags &&\
  git checkout ${IOHK_LIBSODIUM_GIT_REV} \
  && ./autogen.sh && \
  ./configure --prefix=/usr && \
  make && \
  make install && cd .. && rm -rf ./libsodium

# install ghcup
ENV PATH=${PATH}:${HOME:-/root}/.ghcup/bin
RUN wget --secure-protocol=TLSv1_2 \
  https://downloads.haskell.org/~ghcup/$(arch)-linux-ghcup \ 
  && chmod +x $(arch)-linux-ghcup \
  && mkdir -p ${HOME:-/root}/.ghcup/bin \
  && mv $(arch)-linux-ghcup ${HOME:-/root}/.ghcup/bin/ghcup 

# install ghc, caball, and hls
RUN ghcup config set downloader Wget \
  &&  ghcup install ghc ${GHC_VERSION} \
  &&  ghcup install cabal ${CABAL_VERSION}
RUN ghcup set ghc ${GHC_VERSION}
RUN ghcup install hls ${HLS_VERSION}

# Add cabal to PATH
RUN echo "export PATH=$PATH:/root/.cabal/bin" >> ~/.bashrc

ARG CARDANO_NODE_VERSION
ARG CARDANO_CLI_VERSION
ARG STYLISH_HASKELL_VERSION
ARG NODE_VERSION
FROM haskell AS cardano_cli

# install cardano-cli (we use a different docker container for the actual node)
RUN wget https://github.com/IntersectMBO/cardano-cli/releases/download/cardano-cli-${CARDANO_CLI_VERSION}/cardano-cli-${CARDANO_CLI_VERSION}-$(arch)-linux.tar.gz \
    && tar -xzvf cardano-cli-${CARDANO_CLI_VERSION}-$(arch)-linux.tar.gz \
    && mv cardano-cli-$(arch)-linux /usr/bin/cardano-cli \
    && chmod +x /usr/bin/cardano-cli \
    && rm -rf cardano-cli-${CARDANO_CLI_VERSION}-$(arch)-linux.tar.gz

# node socket
RUN echo "export CARDANO_NODE_SOCKET_PATH=/root/.cardano/preview/node.socket" >> ~/.bashrc
RUN echo "export NETWORK=preview" >> ~/.bashrc

FROM cardano_cli AS kuber

# install kuber
RUN wget https://github.com/rober-m/kuber/releases/download/babbage-era/$(arch)-linux-kuber
RUN chmod +x $(arch)-linux-kuber
RUN mv $(arch)-linux-kuber /usr/bin/kuber


FROM kuber AS stylish_haskell

# install stylish-haskell (FIXME: couldn't compile a bin for arm64)
# RUN wget https://github.com/rober-m/stylish-haskell/releases/download/v0.14.3.0/$(arch)-linux-stylish-haskell
# RUN chmod +x $(arch)-linux-stylish-haskell
# RUN mv $(arch)-linux-stylish-haskell /usr/bin/stylish-haskell
RUN wget https://github.com/rober-m/stylish-haskell/releases/download/v${STYLISH_HASKELL_VERSION}/x86_64-linux-stylish-haskell
RUN chmod +x x86_64-linux-stylish-haskell
RUN mv x86_64-linux-stylish-haskell /usr/bin/stylish-haskell

FROM kuber AS nodejs

RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
ENV NVM_DIR=/root/.nvm
ENV NODE_VERSION=${NODE_VERSION}
RUN . "$NVM_DIR/nvm.sh" && nvm install ${NODE_VERSION}
RUN . "$NVM_DIR/nvm.sh" && nvm use v${NODE_VERSION}
RUN . "$NVM_DIR/nvm.sh" && nvm alias default v${NODE_VERSION}
ENV PATH="/root/.nvm/versions/node/v${NODE_VERSION}/bin/:${PATH}"
RUN node --version
RUN npm --version

FROM nodejs AS plutus

ARG PPP=plutus-pioneer-program
RUN mkdir ${PPP} && cd ${PPP} && git init &&\
  git remote add origin https://github.com/input-output-hk/plutus-pioneer-program &&\
  git fetch --depth 1

RUN echo "alias serve-docs='python3 -m http.server -d /workspace/docs/plutus-docs/haddock/'" >> ~/.bashrc
RUN echo "source <(cardano-cli --bash-completion-script cardano-cli)" >> ~/.bashrc

FROM plutus AS cargo

RUN curl https://sh.rustup.rs -sSfo rustup-init
RUN chmod +x rustup-init
RUN ./rustup-init -y

FROM cargo AS deno

RUN /root/.cargo/bin/cargo install deno --locked

FROM deno AS testing-docs

RUN /root/.cargo/bin/cargo install mdbook
RUN echo "alias serve-testing-docs='mdbook serve /workspace/docs/psm-docs'" >> ~/.bashrc

# Update cabal
#RUN cabal update
#RUN cd plutus-pioneer-program/code && cabal update
#RUN cd plutus-pioneer-program/code && cabal build all
