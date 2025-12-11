open Utils

type graph = { edges: int list array; flipped: int list array }
type checkpoint = { node: int; ancestors: bool array }

let you = 0
let out = 1
let svr = 2
let dac = 3
let fft = 4

let flip_edges edges =
    let n = Array.length edges in
    let flipped = Array.make n [] in

    Array.iteri (fun i -> List.iter (fun j -> flipped.(j) <- i :: flipped.(j))) edges;

    flipped

let parse input =
    let input = trim_end_nl input in
    let name_tbl = Hashtbl.create 100 in
    let id_of_name name = match Hashtbl.find_opt name_tbl name with
        | Some id -> id
        | None -> (let id = Hashtbl.length name_tbl in Hashtbl.add name_tbl name id; id)
    in

    ignore @@ id_of_name "you";
    ignore @@ id_of_name "out";
    ignore @@ id_of_name "svr";
    ignore @@ id_of_name "dac";
    ignore @@ id_of_name "fft";

    let edges = String.split_on_char '\n' input 
        |> List.map (fun l -> 
            let self, b = string_split_once ": " l in
            let outputs = String.split_on_char ' ' b in
            id_of_name self, List.map id_of_name outputs) in
    let graph = Array.make (Hashtbl.length name_tbl) [] in

    List.iter (fun (self, outputs) -> 
        graph.(self) <- outputs @ graph.(self)
    ) edges;

    { edges = graph; flipped = flip_edges graph }

let children edges node =
    let n = Array.length edges in
    let visited = Array.make n false in

    visited.(node) <- true;

    let rec explore i = List.iter (fun n ->
        if not visited.(n) then begin
            visited.(n) <- true;
            explore n 
        end) edges.(i)
    in

    explore node;

    visited

let checkpoint_of graph node = { node = node; ancestors = children graph.flipped node }

let count_paths graph goal from =
    let n = Array.length graph.edges in
    let memo = Array.make n (-1) in

    let rec aux i =
        if memo.(i) <> -1 then memo.(i)
        else begin
            let res = 
                if i = goal.node then 1
                else if not goal.ancestors.(i) then 0
                else List.map aux graph.edges.(i) |> List.fold_left (+) 0
            in
            memo.(i) <- res;
            res
        end
    in
    aux from

let solve_part1 graph = count_paths graph (checkpoint_of graph out) you
let solve_part2 graph =
    let checkpoints = Array.map (checkpoint_of graph) [|fft; dac; out|] in

    Array.sort (fun i j -> if j.ancestors.(i.node) then -1 else 1) checkpoints;

    Array.fold_left (fun (n, from) goal -> n * (count_paths graph goal from), goal.node) (1, svr) checkpoints |> fst

let day11 input =
    let graph = parse input in
    let part1 = solve_part1 graph in
    let part2 = solve_part2 graph in
    (string_of_int part1, string_of_int part2)
