open Utils

let parse input =
    let input = trim_end_nl input in
    let ranges, ids = string_split_once "\n\n" input in
    String.split_on_char '\n' ranges
        |> List.map (fun l -> 
            let min, max = string_split_once "-" l in
            (int_of_string min, int_of_string max)),
    String.split_on_char '\n' ids
        |> List.map int_of_string

let collapse ranges =
    let sorted = List.sort (fun (a, _) (b, _) -> compare a b) ((Int.max_int, Int.max_int) :: ranges) in
    let (collapsed, _, _) = List.fold_left (fun (acc, lstart, lend) (cstart, cend) -> 
        if cstart > lend + 1 then ((lstart, lend) :: acc, cstart, cend)
        else (acc, lstart, max lend cend)) ([], -1, -1) sorted
    in
    List.filter (fun r -> r <> (-1, -1) && r <> (Int.max_int, Int.max_int)) collapsed
        |> List.rev |> Array.of_list

let ranges_contain ranges id =
    let len = Array.length ranges in
    let rec aux imin imax =
        if imin >= imax then imin else
        let i = (imin + imax + 1) / 2 in
        if fst ranges.(i) > id then aux imin (i - 1)
        else aux i imax
    in
    let (range_start, range_end) = ranges.(aux 0 (len - 1)) in
    id <= range_end && id >= range_start

let solve_part1 ranges ids =
    List.filter (ranges_contain ranges) ids |> List.length

let solve_part2 =
    Array.fold_left (fun acc (s, e) -> acc + e - s + 1) 0

let day05 input =
    let ranges, ids = parse input in
    let ranges = collapse ranges in
    let part1 = solve_part1 ranges ids in
    let part2 = solve_part2 ranges in
    (string_of_int part1, string_of_int part2)
