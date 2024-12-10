open Utils

let parse input =
  let lines = String.split_on_char '\n' input in
  let height = List.length lines - 1 in
  let width = String.length @@ List.hd lines in
  let grid = Array.make_matrix width height 0 in
  List.iteri
    (fun y l ->
      String.iteri
        (fun x c -> grid.(x).(y) <- int_of_char c - int_of_char '0')
        l)
    lines;
  grid

let score_and_rating grid (x, y) =
  let w, h = grid_dim grid in
  let open Iter in
  let rec aux (x, y) =
    let v = grid.(x).(y) in
    if v = 9 then singleton (x, y)
    else
      of_array [| (x - 1, y); (x, y - 1); (x + 1, y); (x, y + 1) |]
      |> filter (fun (x, y) ->
             x >= 0 && y >= 0 && x < w && y < h && grid.(x).(y) = v + 1)
      |> flat_map aux
  in
  if grid.(x).(y) != 0 then (0, 0)
  else
    let ends = aux (x, y) in
    (group_by ends |> length, length ends)

let solve grid =
  let open Iter in
  let w, h = grid_dim grid in
  product (0 -- (w - 1)) (0 -- (h - 1))
  |> map (score_and_rating grid)
  |> fold (fun (a, b) (c, d) -> (a + c, b + d)) (0, 0)

let day10 input =
  let grid = parse input in
  let part1, part2 = solve grid in
  (string_of_int @@ part1, string_of_int @@ part2)
