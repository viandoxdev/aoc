LDFLAGS=-lcurl
CPPFLAGS=-std=c++20 -O3 -Wall -Wextra -Wpedantic
BIN=bin
OBJECTS:=$(patsubst %.cpp,%.o,$(wildcard *.cpp))

CC=g++

run: all
	./$(BIN)

build: $(BIN)

all: $(BIN)

%.o: %.cpp
	$(CC) -c $(CPPFLAGS) $< -o $@

$(BIN): $(OBJECTS)
	$(CC) $^ $(LDFLAGS) -o $@

clean:
	rm -f *.o
	rm -f *.mod
	rm -f $(BIN)

.PHONY: clean build all run
