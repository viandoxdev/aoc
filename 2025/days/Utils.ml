module I64 = Ocaml_intrinsics.Int64

let int_of_bool = function
    | true -> 1
    | false -> 0

let powers_of_10 = [|1;10;100;1000;10000;100000;1000000;10000000;100000000;1000000000;10000000000;100000000000;1000000000000;10000000000000;100000000000000;1000000000000000;10000000000000000;100000000000000000;1000000000000000000;|]

let p10 = function
    | p when p < 0 -> 0 
    | p -> powers_of_10.(p)
let ilog2 x = 63 - I64.count_leading_zeros (Int64.of_int x)
let ilog10 x = 
    let guesses = [|0;0;0;0;1;1;1;2;2;2;3;3;3;3;4;4;4;5;5;5;6;6;6;6;7;7;7;8;8;8;9;9|] in
    let guess = guesses.(ilog2 x) in
    guess + int_of_bool (x >= powers_of_10.(guess + 1))

let rec sum_range a b f =
    if a <= b then
        f a + sum_range (a + 1) b f
    else 0

let trim_end_nl s = String.sub s 0 (String.length s - 1)

let rec fold_range f acc range_start range_end =
    if range_start > range_end then acc else
    fold_range f (f acc range_start) (range_start + 1) range_end
let rec fold_range_rev f acc range_start range_end =
    if range_start > range_end then acc else
    fold_range_rev f (f acc range_end) range_start (range_end - 1)

let sum = List.fold_left (+) 0
