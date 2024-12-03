open Angstrom
open Utils

type ins = Mul of (int * int) | Enable | Disable | Corrupted

let parse_mul =
  let parse_int_tuple = both digits (char ',' *> digits) in
  string "mul(" *> parse_int_tuple <* char ')' >>| fun m -> Mul m

let parse_enable = string "do()" *> return Enable
let parse_disable = string "don't()" *> return Disable
let parse_corrupted = any_char *> return Corrupted

let parse_ins =
  many (choice [ parse_mul; parse_disable; parse_enable; parse_corrupted ])

let parse input = Result.get_ok @@ parse_string ~consume:All parse_ins input

let part1 ins =
  let count = function Mul (a, b) -> a * b | _ -> 0 in
  sum @@ List.map count ins

let part2 ins =
  let count state i =
    match (state, i) with
    | (true, acc), Mul (a, b) -> (true, acc + (a * b))
    | (_, acc), Enable -> (true, acc)
    | (_, acc), Disable -> (false, acc)
    | (enabled, acc), _ -> (enabled, acc)
  in
  let _, count = List.fold_left count (true, 0) ins in
  count

let day03 input =
  let ins = parse input in
  (string_of_int @@ part1 ins, string_of_int @@ part2 ins)
