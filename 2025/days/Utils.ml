module I64 = Ocaml_intrinsics.Int64

let int_of_bool = function
    | true -> 1
    | false -> 0

let powers_of_10 = [|1;10;100;1000;10000;100000;1000000;10000000;100000000;1000000000;10000000000|]

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
