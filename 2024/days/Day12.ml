open Utils

type cell = Outside | Plot of char

let get_at grid (x, y) =
  try
    let r, _ = grid.(x).(y) in
    r
  with Invalid_argument _ -> Outside

let parse input =
  let lines = String.split_on_char '\n' input in
  let height = List.length lines - 1 in
  let width = String.length @@ List.hd lines in
  let grid = Array.make_matrix width height (Outside, 0) in
  List.iteri
    (fun y l -> String.iteri (fun x c -> grid.(x).(y) <- (Plot c, 0)) l)
    lines;
  Iter.(
    product (0 -- (width - 1)) (0 -- (height - 1))
    |> iter (fun (x, y) ->
           let c, _ = grid.(x).(y) in
           let b (a, b) = int_of_bool (get_at grid (x + a, y + b) <> c) in
           let open Int in
           grid.(x).(y) <-
             ( c,
               of_list [ (1, 0); (0, 1); (-1, 0); (0, -1) ]
               |> map b
               |> fold (fun acc b -> logor (shift_left acc 1) b) 0 )));
  grid

let edge_count e = Ocaml_intrinsics.Int.count_set_bits e

let add_edges t (x, y, e) =
  let open Iter in
  let open Int in
  0 -- 3
  |> fold
       (fun t s -> if logand (shift_right e s) 1 = 1 then (x, y, s) :: t else t)
       t

let edge_hash (x, y, s) = if s mod 2 = 0 then (y * 4) + s else (x * 4) + s

let edge_eq (x1, y1, s1) (x2, y2, s2) =
  if s1 != s2 then false else if s1 mod 2 = 0 then y1 = y2 else x1 = x2

let sides_from_edge_list l =
  let open Iter in
  of_list l
  |> group_by ~hash:edge_hash ~eq:edge_eq
  |> map
       ( List.sort (fun (x1, y1, s) (x2, y2, _) ->
             if s mod 2 = 0 then Int.compare x1 x2 else Int.compare y1 y2)
       %> of_list
       %> map (fun (x, y, s) -> if s mod 2 = 0 then x else y)
       %> fold
            (fun (s, p) c -> if c = p + 1 then (s, c) else (s + 1, c))
            (0, Int.min_int)
       %> fun (s, _) -> s )
  |> sum

let explore grid =
  let w, h = grid_dim grid in
  let opened = Array.make_matrix w h false in
  let open Iter in
  let rec walk c (a, p, t) (x, y) =
    opened.(x).(y) <- true;
    let _, e = grid.(x).(y) in
    of_list [ (x + 1, y); (x, y + 1); (x - 1, y); (x, y - 1) ]
    |> filter (fun (x, y) -> get_at grid (x, y) = c && not opened.(x).(y))
    |> fold (walk c) (a + 1, p + edge_count e, add_edges t (x, y, e))
  in

  Iter.(
    product (0 -- (w - 1)) (0 -- (h - 1))
    |> map (fun (x, y) ->
           let c, _ = grid.(x).(y) in
           if not opened.(x).(y) then
             let area, perimeter, edges = walk c (0, 0, []) (x, y) in
             let sides = sides_from_edge_list edges in
             (area * perimeter, area * sides)
           else (0, 0))
    |> fold (fun (a, b) (c, d) -> (a + c, b + d)) (0, 0))

let day12 input =
  let grid = parse input in
  let part1, part2 = explore grid in
  (string_of_int @@ part1, string_of_int @@ part2)
