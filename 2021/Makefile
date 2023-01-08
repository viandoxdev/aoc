CC=ghc
FLAGS=-funfolding-use-threshold=16 -O2 -optc-O3 -Wall
FLAGS:=$(FLAGS) -prof -fprof-auto
BUILD_DIR=./build
BIN=./aoc2021
ARGS=+RTS -p

SOURCES:=$(wildcard *.hs */*.hs)

run: $(BIN)
	@echo
	$(BIN) $(ARGS)
clean:
	rm -rf $(BUILD_DIR)
	rm -rf ./inputs
	rm -f $(BIN)

$(BIN): $(SOURCES) | directories
	ghc --make $(FLAGS) Main.hs -odir $(BUILD_DIR) -hidir $(BUILD_DIR) -o $@

directories: $(BUILD_DIR) ./inputs

.PRECIOUS: $(BUILD_DIR) ./inputs
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)
./inputs:
	mkdir -p ./inputs

.PHONY: run clean directories
