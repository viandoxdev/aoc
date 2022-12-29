CC=ghc

run: bin
	./bin

build: bin

bin: main.hs
	$(CC) $< -o $@

.PHONY: run build
