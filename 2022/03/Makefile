ASFLAGS=-g
LDFLAGS=--nostd
BIN=bin
OBJECTS:=$(patsubst %.s,%.o,$(wildcard *.s))

AS=as
LD=ld

run: input all
	./$(BIN)

build: input $(BIN)

all: $(BIN)

%.o: %.s
	$(AS) $(ASFLAGS) $< -o $@

$(BIN): $(OBJECTS)
	$(LD) $^ $(LDFLAGS) -o $@

input:
	curl --cookie "session=$$(cat ../../session)" "https://adventofcode.com/2022/day/3/input" -o input

clean:
	rm input

.PHONY: clean build all run
