import strutils
import std/httpclient
import std/intsets
import std/sequtils
import std/sugar
import unicode

proc getInput(): string =
  let session = readFile("../../session").splitLines()[0]
  let client = newHttpClient()
  client.headers = newHttpHeaders({ "Cookie": "session=" & session })
  return client.getContent("https://adventofcode.com/2022/day/6/input")

proc firstMarker(input: string, size: int): int =
  for i in 0..<(input.len - size):
    let chunk = input[i..<(i+size)].map(x => (int)x)
    if chunk.toPackedSet.len == size:
      return i + size # plus size because we want the index of the last character

let input = getInput()
let part1 = input.firstMarker(4)
let part2 = input.firstMarker(14)
echo "Part 1: " & $part1
echo "Part 2: " & $part2
