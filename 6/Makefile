NIMFLAGS=-d:ssl

run: main
	./main

build: main

main: main.nim
	nim c $(NIMFLAGS) $^

.PHONY: run build
