# Advent of Code 2021 solutions

AOC 2021 solutions in haskell

## Running

Using the makefile (yeah I could have just made a cabal/stack project, but whatever)

## Deps 

Build dependencies

- make
- ghc

The rest are just haskage packages.

"Framework" dependencies, used to programatically get the inputs:

- utf8-string (used to decode inputs)
- http-client (used to get inputs)
- http-client-tls
- http-types

Challenges dependencies (I tried to use as little as possible)

- array (STArray for optimizations)
