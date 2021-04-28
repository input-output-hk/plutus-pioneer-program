FROM haskell:latest

COPY . .

RUN cd code/week03 && cabal install

CMD ["cabal", "repl"]
