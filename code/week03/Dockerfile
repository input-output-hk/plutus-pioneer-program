FROM haskell:latest

COPY . .

RUN cabal install

CMD ["cabal", "repl"]
