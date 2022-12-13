open Lwt
open Cohttp_lwt_unix

let session () = 
  let ic = open_in "../session" in
  try 
    input_line ic
  with e ->
    close_in_noerr ic;
    raise e

let input session =
  let uri = Uri.of_string "https://adventofcode.com/2022/day/13/input" in 
  let headers = Cohttp.Header.init_with "Cookie" ("session=" ^ session) in
  Client.get ~headers uri >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
      body

type item = 
  | Single of int 
  | List of item list

type token = LStart | LEnd | Num of int

let lex_line line =
  Str.full_split (Str.regexp "[],[]") line
    |> List.filter_map (
      function
        | Str.Text n -> Some (Num (int_of_string n))
        | Str.Delim "[" -> Some( LStart)
        | Str.Delim "]" -> Some( LEnd)
        | _ -> None
      )

exception NotList

let cons item list = match list with
  | List l -> List(item :: l)
  | _ -> raise NotList

let rev = function
  | List l -> List(List.rev l)
  | x -> x

let hd = function
  | List l -> List.hd l
  | x -> x

let parse_line line =
  let tokens = lex_line line in
  let fold (state: item list) (token: token) = match token with
    | LStart -> List([]) :: state
    | LEnd -> (cons (List.hd state |> rev) (state |> List.tl |> List.hd)) :: (state |> List.tl |> List.tl)
    | Num n -> (cons (Single (n)) (List.hd state)) :: (List.tl state)
  in
  List.fold_left fold [List([])] tokens |> List.hd |> hd

let parse_input1 input =
  let pairs = Str.split (Str.regexp "\n\n") input in
  let parse_pair pair = 
    let lines = String.split_on_char '\n' pair in
    ((List.nth lines 0 |> parse_line), (List.nth lines 1 |> parse_line))
  in
  List.map parse_pair pairs

let parse_input2 input =
  let lines = String.split_on_char '\n' input in
  List.filter (fun line -> line <> "") lines
    |> List.map parse_line

type order = Unknown | Correct | Wrong

exception Unreachable

let rec list_order a b prev = match (a, b, prev) with
  | (_, _, Correct) -> Correct
  | (_, _, Wrong) -> Wrong
  | ([], [], Unknown) -> Unknown
  | ([], _, Unknown) -> Correct
  | (_, [], Unknown) -> Wrong
  | (_, _, Unknown) -> list_order (List.tl a) (List.tl b) (item_order (List.hd a) (List.hd b))
and item_order a b = match a with
  | Single x -> (match b with
    | Single y when x < y -> Correct
    | Single y when x = y -> Unknown
    | Single y when x > y -> Wrong
    | Single _ -> raise Unreachable (* for exhaustivity *)
    | List _ -> item_order (List([a])) b)
  | List x -> (match b with
    | Single _ -> item_order a (List([b]))
    | List y -> list_order x y Unknown)

let part1 sinput =
  let input = parse_input1 sinput in
  let proc i (a, b) =
    match (item_order a b) with
      | Correct -> i + 1
      | _ -> 0
  in
  List.mapi proc input |> List.fold_left (+) 0

let part2 sinput =
  let input = parse_input2 sinput |> List.append [List([List([Single(2)])]); List([List([Single(6)])])] in
  let sorted = List.sort (fun a b -> match (item_order a b) with 
    | Correct -> -1
    | Unknown -> 0
    | Wrong -> 1) input
    |> List.mapi (fun i e -> (i + 1, e))
  in
  let (div1, _) = List.find (fun (_, e) -> match e with 
    | List([List([Single(2)])]) -> true
    | _ -> false) sorted
  in
  let (div2, _) = List.find (fun (_, e) -> match e with 
    | List([List([Single(6)])]) -> true
    | _ -> false) sorted
  in
  div1 * div2

let () =
  let session = session () in
  let string_input = Lwt_main.run (input session) in
  let p1 = part1 string_input in
  let p2 = part2 string_input in
  print_endline ("\n\nPart1: " ^ (string_of_int p1));
  print_endline ("Part2: " ^ (string_of_int p2))
