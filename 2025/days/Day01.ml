open Utils

let (%%) x y = 
    let res = x mod y in
    if res >= 0 then res else res + y

let parse input = 
    let input = String.sub input 0 (String.length input - 1) in
    String.split_on_char '\n' input |> List.map (fun s -> 
        let len = String.length s in
        let sign = if s.[0] == 'R' then 1 else -1 in
        let value = int_of_string (String.sub s 1 (len - 1)) in
        (sign, value)
    )

let solve_part1 clicks = 
    List.fold_left (fun (p, a) (sign, value) -> 
        match ((a + sign * value) mod 100) with
            | 0 -> (p + 1, 0)
            | a -> (p, a)) (0, 50) clicks |> fst

let solve_part2 clicks = 
    List.fold_left (fun (p, a) (sign, value) -> 
        let na = (a + (value mod 100) * sign) in
        let np = p + value / 100 + int_of_bool ((a > 0 && na <= 0) || na >= 100) in
        (np, na %% 100)
        )
    (0, 50) clicks |> fst

let day01 input =
    let clicks = parse input in
    let part1 = solve_part1 clicks in 
    let part2 = solve_part2 clicks in 
  (string_of_int part1, string_of_int part2)
