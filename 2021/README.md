# Advent of Code 2021 solutions

AOC 2021 solutions in haskell

## Running

Using the makefile (yeah I could have just made a cabal/stack project, but whatever)

## Deps 

Build dependencies

- make
- ghc

The rest are just haskage packages.

"Framework" dependencies, used to programatically get the inputs, and measure runtime:

- utf8-string
- http-client
- http-client-tls
- http-types
- clock

Challenges dependencies (I tried to use as little as possible)

- array (STArray for optimizations)
