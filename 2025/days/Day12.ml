open Utils

type tree = { width: int; height: int; presents: int list; }

let parse_present =
    String.fold_left (fun a -> function
        | '#' -> a * 2 + 1
        | '.' -> a * 2
        | _ -> a
    ) 0 
let parse_trees tree =
    let width, tree = string_split_once "x" tree in
    let height, presents = string_split_once ": " tree in

    { width = int_of_string width
    ; height = int_of_string height
    ; presents = String.split_on_char ' ' presents |> List.map int_of_string }

let parse input =
    let input = trim_end_nl input in
    let presents, trees = match string_split "\n\n" input |> List.rev with
        | trees :: presents -> List.rev presents, String.split_on_char '\n' trees
        | [] -> failwith "bad input" in

    let presents = List.map parse_present presents |> Array.of_list in
    let trees = List.map parse_trees trees in

    presents, trees

let is_conclusively_out presents tree =
    tree.width * tree.height 
        < (List.mapi (fun i n -> n * Ocaml_intrinsics.Int.count_set_bits presents.(i)) tree.presents |> List.fold_left (+) 0)

let is_conclusively_in presents tree =
    let count = List.fold_left (+) 0 tree.presents in
    (tree.width / 3) * (tree.height / 3) >= count

let is_in presents tree =
    failwith "Unimplemented: my input doesn't have this case :/"

let fits presents tree =
    is_conclusively_in presents tree || ((not @@ is_conclusively_out presents tree) && is_in presents tree)

let solve presents trees = 
    List.length @@ List.filter (fits presents) trees

let day12 input =
    let presents, trees = parse input in
    let answer = solve presents trees in
    (string_of_int answer, "")
