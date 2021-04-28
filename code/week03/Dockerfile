FROM haskell:8

COPY . .
RUN ls
WORKDIR code/week03
RUN ls
RUN cabal update && cabal install

CMD ["cabal", "repl"]
