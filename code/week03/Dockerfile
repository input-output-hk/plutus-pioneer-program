FROM haskell:8
RUN apt-get update -y && apt-get install -y pkg-config

COPY . .
RUN ls
WORKDIR code/week03
RUN ls
RUN cabal update && cabal install

CMD ["cabal", "repl"]
