open Utils

let digit_to_int c = int_of_char c - int_of_char '0'

let max_digit_index s = 
    let len = String.length s in
    range_fold_rev (fun (j, m) i -> 
        let v = digit_to_int s.[i] in
        if v >= m then (i, v) else (j, m)
    ) (0, digit_to_int s.[0]) 0 (len - 1)

let rec max_joltage_n n line = let len = String.length line in match n with
    | 0 -> 0
    | n when n = len -> int_of_string line
    | n -> (
        let (i, d) = max_digit_index (String.sub line 0 (len - n + 1)) in
        d * (p10 (n - 1)) + max_joltage_n (n - 1) (String.sub line (i + 1) (len - i - 1)))

let solve n lines =
    List.map (max_joltage_n n) lines |> sum

let day03 input =
    let lines = String.split_on_char '\n' (trim_end_nl input) in
    let part1 = solve 2 lines in
    let part2 = solve 12 lines in
    (string_of_int part1, string_of_int part2)
