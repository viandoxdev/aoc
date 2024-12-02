open Angstrom
open Utils

let parse_line =
  digits <* whitespace
  >>= fun left -> 
    digits >>= fun right -> return (left, right)
  <* char '\n'

let parse input = 
  List.split @@ Result.get_ok @@ Angstrom.parse_string ~consume: All (many1 parse_line) input

let part1 left right = 
  let sorted_left = List.sort compare left in
  let sorted_right = List.sort compare right in
  sum @@ List.map2 (fun a b -> abs @@ a - b) sorted_left sorted_right

let part2 left right =
  List.fold_left (+) 0 @@ List.map (fun a -> 
    let occ = List.length @@ List.filter (fun b -> a = b) right in
    occ * a
  ) left

let day01 input =
  let (left, right) = parse input in
  (string_of_int @@ part1 left right, string_of_int @@ part2 left right)
