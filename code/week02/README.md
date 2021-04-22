# Lecture 2 notes
Lecture notes based on the first ever official [ Plutus-Pioneer program](https://github.com/input-output-hk/plutus-pioneer-program). This notes follow the [YouTube lecture 2](https://www.youtube.com/watch?v=E5KRk5y9KjQ/).

[[get-started]]
## 1. Item 1 Get started with the cabal repl

Since we are not installing cabal locally, bacause it is much more convenient to use it from nix-shell (this is included in [Nix](https://nixos.org/nix/)), we will navigate to the plutus-pioneer/week02 git-cloned repo inside a nix environment. However, nix must be started first from the plutus root repo.

    $ cd Cardano-King/plutus/  
    $ nix-shell
    nix$ cd ../plutus-pioneer-program/code/week02
    nix$ cabal repl
    
Now we are isnide the cabal repl (takes a while the first time). Time to test some varaibles defined in the plutus libraries

    repl$ import PlutusTx
    
Some examples seeing the type of Data and create bite-string on the fly (use literal strings to create byte strings), and then test it (`:t B ...`)
    
    repl$ :i Data
    repl$ :set -XOverloadedStrings  ## allows to include extremely large strings (literal-string to byte-strings)
    repl$ :t B "Haskellanians"
    

[[first-validator]]
## 1. Item 2 Implement our first validator
Start a Haskell module "Week02.Gift" where the script imports all the necessary language extensions that plutus needs. (first 8 lines `{-#...#-}`) and other modules that we refer to (lines 12-28) from the `./week01/EnglishAuction.hs` script.

Go back to the repl and load Gift.hs. Then we will see what a Monoid in haskell `()`

    repl$ :l ./src/Week02/Gift.hs 
    repl$ :t ()
    
We can open Gift.hs in a text editor and see that lines 31-35 contain the implementation of Monoid referring to the [get-started] section above.
    

 
