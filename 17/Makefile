LDFLAGS=
FFLAGS=-g -fno-range-check
BIN=bin
OBJECTS:=$(patsubst %.f95,%.o,$(wildcard *.f95))

CC=gfortran

run: input all
	./$(BIN)

build: input $(BIN)

all: $(BIN)

%.o: %.f95
	$(CC) -c $(FFLAGS) $< -o $@

$(BIN): $(OBJECTS)
	$(CC) $^ $(LDFLAGS) -o $@

input:
	curl --cookie "session=$$(cat ../session)" "https://adventofcode.com/2022/day/17/input" -o input

clean:
	rm -f input
	rm -f *.o
	rm -f *.mod
	rm -f $(BIN)

.PHONY: clean build all run
