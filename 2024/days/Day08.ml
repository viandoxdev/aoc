open Utils.Point

let parse input =
  let open Iter in
  let lines = String.split_on_char '\n' input in
  let w, h = (String.length @@ List.hd lines, List.length lines - 1) in
  ( of_list lines
    |> mapi (fun y l ->
           of_str l
           |> filter_mapi (fun x c ->
                  if c != '.' then Some (c, (x, y)) else None))
    |> flatten
    |> group_by
         ~hash:(fun (a, _) -> int_of_char a)
         ~eq:(fun (a, _) (b, _) -> a = b)
    |> map (fun l ->
           let c, _ = List.hd l in
           (c, List.map (fun (_, p) -> p) l))
    |> to_list,
    w,
    h )

let is_valid w h (x, y) = x >= 0 && y >= 0 && x < w && y < h

let antinodes1 _ _ (a, b) =
  Iter.doubleton (sub (scale 2 a) b) (sub (scale 2 b) a)

let antinodes2 w h (a, b) =
  let open Iter in
  let ab, ba = (sub b a, sub a b) in
  append
    (iterate (add ab) a |> take_while (is_valid w h))
    (iterate (add ba) b |> take_while (is_valid w h))

let part f antennas w h =
  let open Iter in
  of_list antennas
  |> flat_map (fun (_, l) -> diagonal_l l |> flat_map (f w h))
  |> filter (is_valid w h)
  |> sort_uniq |> length

let part1 = part antinodes1
let part2 = part antinodes2

let day08 input =
  let antennas, w, h = parse input in
  (string_of_int @@ part1 antennas w h, string_of_int @@ part2 antennas w h)
