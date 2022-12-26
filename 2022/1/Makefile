CC=gcc

part1: 1.sh
	./1.sh

part2: 2
	./2

2: 2.c
	$(CC) 2.c -lcurl -o 2

clean:
	rm -rf 2

.PHONY: clean part1 part2
