open Utils

type key =
  | N0
  | N1
  | N2
  | N3
  | N4
  | N5
  | N6
  | N7
  | N8
  | N9
  | A
  | U
  | L
  | D
  | R

type keypad = { coords: (int * int) option array; path_to: int * int -> int * int -> (int * key) Iter.t }

let key_of_char = function
  | '0' -> N0
  | '1' -> N1
  | '2' -> N2
  | '3' -> N3
  | '4' -> N4
  | '5' -> N5
  | '6' -> N6
  | '7' -> N7
  | '8' -> N8
  | '9' -> N9
  | 'A' -> A
  | _ -> failwith "No such key"

let char_of_key = function
  | N0 -> '0'
  | N1 -> '1'
  | N2 -> '2'
  | N3 -> '3'
  | N4 -> '4'
  | N5 -> '5'
  | N6 -> '6'
  | N7 -> '7'
  | N8 -> '8'
  | N9 -> '9'
  | A -> 'A'
  | U -> '^'
  | L -> '<'
  | R -> '>'
  | D -> 'v'

let int_of_key = function
  | N0 -> 0
  | N1 -> 1
  | N2 -> 2
  | N3 -> 3
  | N4 -> 4
  | N5 -> 5
  | N6 -> 6
  | N7 -> 7
  | N8 -> 8
  | N9 -> 9
  | A -> 10
  | U -> 11
  | L -> 12
  | D -> 13
  | R -> 14

let dist (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)

let parse input =
  String.split_on_char '\n' input
  |> List.filter ((<>) "")
  |> List.map
       Iter.(
         fun s ->
           ( of_str s |> map key_of_char |> to_list,
             int_of_string (of_str s |> filter is_digit |> to_str) ))

let keypad_coords l =
  let a = Array.make 15 None in
  List.iter (fun (k, x, y) -> a.(int_of_key k) <- Some (x, y)) l;
  a

let path_to_numeric (sx, sy) (ex, ey) =
  let open Iter in
  let (gx, gy) = (0, 3) in
  let dx, dy = (ex - sx, ey - sy) in
  let hor = if dx < 0 then (abs dx, L) else (dx, R) in
  let ver = if dy < 0 then (abs dy, U) else (dy, D) in

  if dy = 0 then
    singleton hor
  else if dx = 0 then
    singleton ver
  else if dx > 0 then
    doubleton hor ver
  else if dy < 0 && not (gx = sx && gy = ey) then
    doubleton ver hor
  else
    doubleton ver hor

let path_to_directional (sx, sy) (ex, ey) =
  let open Iter in
  let (gx, gy) = (0, 0) in
  let dx, dy = (ex - sx, ey - sy) in
  let hor = if dx < 0 then (abs dx, L) else (dx, R) in
  let ver = if dy < 0 then (abs dy, U) else (dy, D) in

  if dy = 0 then
    singleton hor
  else if dx = 0 then
    singleton ver
  else if dx > 0 then
    doubleton hor ver
  else if dy < 0 && not (gx = sx && gy = ey) then
    doubleton ver hor
  else
    doubleton ver hor

let numeric_pad =
  { coords= keypad_coords
    [
      (N7, 0, 0);
      (N8, 1, 0);
      (N9, 2, 0);
      (N4, 0, 1);
      (N5, 1, 1);
      (N6, 2, 1);
      (N1, 0, 2);
      (N2, 1, 2);
      (N3, 2, 2);
      (N0, 1, 3);
      (A, 2, 3);
    ];
    path_to= path_to_numeric
  }

let directional_pad =
  { coords = keypad_coords
    [ (U, 1, 0); (A, 2, 0); (L, 0, 1); (D, 1, 1); (R, 2, 1) ];
    path_to = path_to_directional
  }

let pad_code code pad =
  let open Iter in
  code
  |> fold_map
       (fun p (m, k) ->
         let n = Option.get @@ pad.coords.(int_of_key k) in
         (n, append (pad.path_to p n) @@ singleton (m, A)))
       (Option.get @@ pad.coords.(int_of_key A))
  |> flatten

let type_code pads code =
  let open Iter in
  List.fold_left pad_code (of_list code |> map (fun k -> (1, k))) pads
    |> map (fun (n, _) -> n)
    |> sum

let string_of_code l =
  Iter.(of_list l |> flat_map (fun (n, k) -> repeat (char_of_key k) |> take n) |> to_str) ^ " " ^ string_of_int Iter.(of_list l |> map (fun (n, _) -> n) |> sum)

let type_code pads code =
  let open Iter in
  List.fold_left (fun c p -> print_endline @@ string_of_code c; to_list (pad_code (of_list c) p)) (on_list (map (fun k -> (1, k))) code) pads
    |> (fun c -> print_endline @@ string_of_code c; c)
    |> of_list
    |> map (fun (n, _) -> n)
    |> sum

let part1 codes =
  let open Iter in
  let pads = [numeric_pad; directional_pad; directional_pad] in
  of_list codes
    |> map (fun (code, n) -> n * type_code pads code)
    |> sum

let day21 input =
  let codes = parse input in
  (string_of_int @@ part1 codes, "TODO")
