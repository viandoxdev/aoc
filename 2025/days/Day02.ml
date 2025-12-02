open Utils

let parse input =
    let len = String.length input in
    String.split_on_char ',' (String.sub input 0 (len - 1))
        |> List.map (fun s -> (
            let l = String.length s in
            let i = String.index_from s 0 '-' in
            (int_of_string (String.sub s 0 i), int_of_string (String.sub s (i+1) (l - i - 1)))
        ))

let rec concat_n n x s =
    if n = 0 then 0
    else (concat_n (n - 1) x s) * s + x

(* Add all the passwords that are a sequence of d digits repeating rep times in the [a; b] range to list *)
let invalid_in_range_length_repeat list a b d rep =
    let shift = p10 d in
    let limit = p10 (d * rep) in
    let start = max (a / (p10 (d * rep - d))) (shift / 10) in
    let rec aux acc x =
        let r = concat_n rep x shift in
        if r <= b && r < limit then if r >= a then (
            aux (r :: acc) (x + 1) 
        ) else aux acc (x + 1) else acc
    in
    aux list start
(* Add all the passwords that are a sequence of d digits repeated at least twice in the [a; b] range to list *)
let invalid_in_range_length ?(part1=true) list a b d =
    let da, db = ilog10 a + 1, ilog10 b + 1 in
    let rep_min = max ((da + d - 1) / d) 2 in
    let rep_max = if part1 then 2 else db / d in
    let rec aux list rep = 
        if rep > rep_max then list
        else aux (invalid_in_range_length_repeat list a b d rep) (rep + 1)
    in
    aux list rep_min

(* Add all the passwords that are a sequence of digits repeated at least twice in the [a; b] range to list *)
let invalid_in_range ?(part1=true) (a, b) = 
    let db = ilog10 b + 1 in
    let rec aux list d = if d > (db / 2) then list else (
        aux (invalid_in_range_length ~part1: part1 list a b d) (d+1)) in
    aux [] 1 |> List.sort_uniq compare |> List.fold_left (+) 0

let solve_part1 = List.fold_left (fun a r -> a + invalid_in_range r) 0
let solve_part2 = List.fold_left (fun a r -> a + invalid_in_range ~part1: false r) 0

let day02 input =
    let ranges = parse input in
    let part1 = solve_part1 ranges in
    let part2 = solve_part2 ranges in
    (string_of_int part1, string_of_int part2)


