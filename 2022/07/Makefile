AWKFLAGS=-c

run: input main.awk
	awk $(AWKFLAGS) -f main.awk < input

input:
	curl --cookie "session=$$(cat ../../session)" "https://adventofcode.com/2022/day/7/input" -o input

clean:
	rm input

.PHONY: clean run
