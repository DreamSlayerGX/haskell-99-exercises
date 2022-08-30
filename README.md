# Haskell Ninety-Nine Problems

Haskell exercises found at https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems.

## Install
Installation for haskell can be found at [haskell downloads](https://www.haskell.org/downloads/) and at [haskell in 5 steps](https://wiki.haskell.org/Haskell_in_5_steps) which also have tutorial recommendations.

## Interactive scripts
Functions written in `myFunction.sh` can be used in the `ghci` by writing `:l myFunctions`. This is a fast way to test our functions instead of compiling them. After changes, one simply reloads the script with `:r`.

## Compile a program
The [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) is used to compile haskell fles. To compile a file, simply write `ghc --make <file>.hs -o main`. The `make` flag tells the compiler that it is a program and thus includes all the modules required.
 
