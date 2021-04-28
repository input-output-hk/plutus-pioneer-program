FROM haskell:latest

COPY . .
WORKDIR code/week03
RUN cabal update && cabal install

CMD ["cabal", "repl"]
