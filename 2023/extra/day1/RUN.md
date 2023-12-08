# Deps

- a brainfuck interpreter/compiler

# Input

Run the rust version first to get the input in ../inputs/1, or just download it wherever

# Note

The program executes 125,644,976,784,310 instructions on my input, so most basic interpreter won't be able to run it in a reasonable time.
The cell size should be at least 16 bit, but 32 can't hurt.
The input should end with a single new line character, not two or more.
Require at least 30 cells, 64 is more than enough.

A few example of working ones:
- [copy.sh's el brainfuck](https://copy.sh/brainfuck/) crazy fast one for a javascript interpreter, probably JIT compiled and optimized by the browser. `awk < input 'ORS="\\n"'` will produce the correct input string.
- [tritium](https://github.com/rdebath/Brainfuck) fastest one I could find, and actually compiles, can be run with `bf main.b -mem 64 -b32 -O3 -E3 < input`
