## Part 1

### Deps

- curl
- nvim

### Running

open the keystrokes file in nvim
```
nvim keystrokes
```

then select and copy its content with `ggVG"ay`, and execute it (in the input file) by running the macro with `@a`.

You can get the result for part 1 in the u register with `:reg u`

## Part 2

While I would have loved to do this one with (n)vim keystrokes too, my current approach is just too slow and I don't think I'll find a much faster alternative that doesn't involve relying heavily on vim script (kindof defeating the point). As a estimate for the time it would've taken with vim: the first problem's answer (for me) was 799, this took a few minutes (lets say 3) to compute. The second problem's answer was 29076, on a considerably larger scene (makes all the substitution commands slower), so about 36 times as much, not counting the slowdown, that gives about 1h30 to compute, now imagine if I had a single bug in that code.

So I made a quick and dirty rust solution, its the same logic anyways, just with different parsing.

### Deps

- rust (cargo)

### running
 
go into the part2 directory and run it.

```
cargo run
```
