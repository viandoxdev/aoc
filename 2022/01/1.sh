#!/bin/bash

session="$(cat ../../session)"

data=$(mktemp)
curl --cookie "session=$session" 'https://adventofcode.com/2022/day/1/input' -o "$data"

elf_total=0
elf_max=0

while read -r line; do
	if [ -z "$line" ]; then
		if [ "$elf_total" -gt "$elf_max" ]; then
			elf_max="$elf_total"
		fi
		elf_total=0
	else
		((elf_total+=line))
	fi
done < "$data"

echo "max: $elf_max"

rm "$data"
