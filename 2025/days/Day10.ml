open Utils

let parse_int_list s = String.split_on_char ',' s |> List.map int_of_string
let parse_lights s =
    String.fold_right (fun c a -> match c with
        | '#' -> 2 * a + 1
        | '.' -> 2 * a
        | _ -> a) s 0
let parse_buttons s =
    String.split_on_char ' ' (trim_end_nl s) 
        |> List.map (fun s -> 
            parse_int_list (String.sub s 1 (String.length s - 2)) 
                |> List.fold_left (fun a d -> a + Int.shift_left 1 d) 0)

let parse input =
    let input = trim_end_nl input in
    String.split_on_char '\n' input 
        |> List.map (fun l -> 
            let a, b = string_split_once " " l in
            let b, c = string_split_once "{" b in
            let c = String.sub c 0 (String.length c - 1) in

            String.length a - 2,
            parse_lights a,
            parse_buttons b |> Array.of_list,
            parse_int_list c |> Array.of_list)

type 'a control = Continue of 'a | Stop of 'a

let select i a =
    let rec aux i o = 
        if i = 0 then [] 
        else if Int.logand i 1 = 1 then a.(o) :: aux (Int.shift_right i 1) (o + 1)
        else aux (Int.shift_right i 1) (o + 1) in
    aux i 0

let bin_of_int n =
    if n = 0 then "0" else
    let rec aux n = if n = 0 then "" else aux (n / 2) ^ if n mod 2 = 0 then "0" else "1" in
    aux n

let turn_on (n, lights, buttons, _) =
    let k = Array.length buttons in
    let m = Int.shift_left 1 k in
    range 0 (m - 1) 
        |> List.filter (fun i -> List.fold_left Int.logxor 0 (select i buttons) = lights)
        |> List.map Ocaml_intrinsics.Int.count_set_bits
        |> List.fold_left min (k + 1)

let fold_bin f acc bin =
    let rec aux off bin acc = 
        if bin = 0 then acc
        else 
            let skip = Ocaml_intrinsics.Int.count_trailing_zeros bin in
            aux (off + skip + 1) (Int.shift_right bin (skip + 1))  (f acc (off + skip))
    in aux 0 bin acc

let string_of_button b = "(" ^ (fold_bin (fun a i -> string_of_int i :: a) [] b |> String.concat ",") ^ ")"
let string_of_buttons bs = String.concat " " @@ List.map string_of_button @@ Array.to_list  bs
let string_of_joltages jolts = "{" ^ (String.concat " " @@ List.map string_of_int @@ Array.to_list jolts) ^ "}"

let push_button b jolts =
    fold_bin (fun _ i -> jolts.(i) <- jolts.(i) - 1) () b

let can_press b jolts = fold_bin (fun f i -> f && jolts.(i) >= 1) true b

let min_joltage (_, _, buttons, joltage) =
    let k = Array.length buttons in
    let constraints = Array.mapi 
        (fun i j -> 
            range 0 (k - 1) 
                |> List.filter (fun l -> Int.logand buttons.(l) (Int.shift_left 1 i) = 1),
            j
        ) joltage in
    failwith ""

let solve_part1 machines =
    List.map turn_on machines |> List.fold_left (+) 0
let solve_part2 machines =
    List.map min_joltage machines |> List.fold_left (+) 0

let day10 input =
    let machines = parse input in
    let part1 = solve_part1 machines in
    let part2 = solve_part2 machines in
    (string_of_int part1, string_of_int part2)
