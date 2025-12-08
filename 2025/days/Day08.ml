open Utils

let parse input =
    trim_end_nl input
        |> String.split_on_char '\n'
        |> List.map (fun l ->
            let a, b = string_split_once "," l in
            let b, c = string_split_once "," b in
            tmap3 int_of_string (a, b, c))
        |> Array.of_list

let sq_dist (ax, ay, az) (bx, by, bz) =
    let dx, dy, dz = ax - bx, ay - by, az - bz in
    dx * dx + dy * dy + dz * dz

let prep verts = 
    let n = Array.length verts in
    let arr = Array.make (n * (n - 1) / 2) (0, 0, 0) in
    
    let rec populate a b i = if a < n - 1 then begin
        if b >= n then populate (a + 1) (a + 2) i
        else begin
            arr.(i) <- (sq_dist verts.(a) verts.(b), a, b);
            populate a (b + 1) (i + 1)
        end
    end in

    populate 0 1 0;
    Array.fast_sort (fun (a, _, _) (b, _, _) -> Int.compare a b) arr;

    Array.to_list arr

let solve_part1 verts pairs = 
    let n = Array.length verts in
    let sets = UnionFind.make n in

    List.take 1000 pairs
        |> List.iter (fun (_, a, b) -> UnionFind.union a b sets);

    UnionFind.sizes sets
        |> List.sort (fun a b -> Int.compare b a)
        |> List.take 3
        |> List.fold_left ( * ) 1

let solve_part2 verts pairs =
    let n = Array.length verts in
    let sets = UnionFind.make n in

    let rec aux (ax, _, _) (bx, _, _) = function
        | _ when UnionFind.components sets = 1 -> ax * bx
        | (_, a, b) :: r -> (UnionFind.union a b sets; aux verts.(a) verts.(b) r)
        | [] -> failwith "Made every connection without connecting everything, interesting."
    in
    aux (0, 0, 0) (0, 0, 0) pairs

let day08 input =
    let verts = parse input in
    let pairs = prep verts in
    let part1 = solve_part1 verts pairs in
    let part2 = solve_part2 verts pairs in
    (string_of_int part1, string_of_int part2)
