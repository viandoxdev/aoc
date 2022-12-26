sbcl: input main.lisp
	sbcl --no-sysinit --no-userinit --script main.lisp
clisp: input main.lisp
	clisp main.lisp
input:
	curl --cookie "session=$$(cat ../../session)" "https://adventofcode.com/2022/day/18/input" -o input
clean:
	rm -f input

.PHONY: sbcl clisp
