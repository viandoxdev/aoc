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

let parse_grid init f input =
    let width = String.index input '\n' in
    let height = String.fold_right (fun c a -> a + int_of_bool (c = '\n')) input 0 in
    let data = init width height in
    ignore @@ String.fold_left (fun (x, y) c -> 
        if c = '\n' then (0, y + 1)
        else (
            f x y c data;
            (x+1, y)
        )
    ) (0, 0) input;
    data

let neighbours_8 = [(-1,-1);(-1,0);(-1,1);(0,-1);(0,1);(1,-1);(1,0);(1,1)]
let inbounds_neighbours w h n x y = 
    List.filter_map (fun (dx, dy) -> 
        let x, y = x + dx, y + dy in
        if x >= 0 && x < w && y >= 0 && y < h then Some (x, y) else None
    ) n

let dim a = (Array.length a.(0), Array.length a)

let copy_grid g = 
    Array.init (Array.length g) (fun y -> Array.copy g.(y))

let string_find_index pat s =
    let patlen, len = String.length pat, String.length s in
    let rec aux i j =
        if j = patlen then i - j
        else if i = len then failwith "Not found"
        else if pat.[j] = s.[i] then aux (i + 1) (j + 1)
        else aux (i + 1) 0
    in
    aux 0 0

let string_split_once pat s =
    let patlen, len = String.length pat, String.length s in
    let i = string_find_index pat s in
    (String.sub s 0 i, String.sub s (i + patlen) (len - i - patlen))

let (<%) f g x = f (g x)
let (%>) f g x = g (f x)

let cons a l = a :: l

let transpose = function
    | [] -> []
    | x::xs -> List.(fold_right (fun l a -> map2 cons l a) (x::xs) (map (fun _ -> []) x) )
