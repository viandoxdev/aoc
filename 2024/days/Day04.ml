open Array
open Utils

type cell = X | M | A | S

let parse input =
  let lines = String.split_on_char '\n' input in
  let width = String.length @@ List.hd lines in
  let height = List.length lines - 1 in
  let grid = make_matrix width height M in
  List.iteri
    (fun y line ->
      String.iteri
        (fun x c ->
          match c with
          | 'X' -> grid.(x).(y) <- X
          | 'A' -> grid.(x).(y) <- A
          | 'S' -> grid.(x).(y) <- S
          | _ -> ())
        line)
    lines;
  grid

let get_grid grid (x, y) =
  let w, h = grid_dim grid in
  if x < 0 || y < 0 || x >= w || y >= h then None else Some grid.(x).(y)

let check_xmas grid ((dx, dy), (x, y)) =
  let word = [ X; M; A; S ] in
  let wordi = List.mapi (fun i c -> (i, c)) word in
  List.for_all
    (fun (i, c) -> get_grid grid (x + (i * dx), y + (i * dy)) = Some c)
    wordi

let check_x_mas grid (x, y) =
  grid.(x).(y) = A
  &&
  let corners =
    [
      grid.(x - 1).(y - 1);
      grid.(x + 1).(y - 1);
      grid.(x + 1).(y + 1);
      grid.(x - 1).(y + 1);
    ]
  in
  let mc = List.(length @@ filter (fun c -> c = M) corners) in
  let sc = List.(length @@ filter (fun c -> c = S) corners) in
  mc = 2 && sc = 2 && List.nth corners 0 != List.nth corners 2

let part1 grid =
  let dirs =
    [ (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1) ]
  in
  let w, h = grid_dim grid in
  Iter.(
    product (of_list dirs) (product (0 -- (w - 1)) (0 -- (h - 1)))
    |> filter (check_xmas grid)
    |> length)

let part2 grid =
  let w, h = grid_dim grid in
  Iter.(
    product (1 -- (w - 2)) (1 -- (h - 2)) |> filter (check_x_mas grid) |> length)

let day04 input =
  let grid = parse input in
  (string_of_int @@ part1 grid, string_of_int @@ part2 grid)
