open Utils

let parse input =
  let lines = String.split_on_char '\n' input in
  let w, h = (String.length @@ List.hd lines, List.length lines - 1) in
  let grid = Array.make_matrix w h true in
  let start_tile = ref (0, 0) in
  let end_tile = ref (0, 0) in
  List.iteri
    (fun y l ->
      String.iteri
        (fun x -> function
          | '#' -> grid.(x).(y) <- false
          | 'S' -> start_tile := (x, y)
          | 'E' -> end_tile := (x, y)
          | _ -> ())
        l)
    lines;
  (grid, !start_tile, !end_tile)

let get_path grid s =
  let w, h = grid_dim grid in
  let dist = Array.make_matrix w h Int.max_int in
  let open Iter in
  let _, _, l =
    init id
    |> fold_while
         (fun ((px, py), ((x, y) as c), l) d ->
           dist.(x).(y) <- d;
           let next =
             of_list [ (0, 1); (1, 0); (-1, 0); (0, -1) ]
             |> map (Point.add c)
             |> filter (fun (x, y) ->
                    x >= 0 && y >= 0 && x < w && y < h
                    && (not (x = px && y = py))
                    && grid.(x).(y))
             |> head
           in
           match next with
           | Some n -> ((c, n, c :: l), `Continue)
           | None -> ((c, c, c :: l), `Stop))
         (s, s, [])
  in
  (l, dist)

let manhattan_dist (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)

let solve ~cheat grid dist path =
  let open Iter in
  let w, h = grid_dim grid in
  of_list path
  |> flat_map (fun ((cx, cy) as c) ->
         let d = dist.(cx).(cy) in
         -cheat -- cheat
         |> flat_map (fun x ->
                let a = abs x in
                a - cheat -- (cheat - a) |> map (fun y -> (x, y)))
         |> map (Point.add c)
         |> filter (fun (x, y) ->
                x >= 0 && y >= 0 && x < w && y < h && grid.(x).(y))
         |> map (fun (x, y) -> d - dist.(x).(y) - manhattan_dist (x, y) c))
  |> filter (( < ) 0)
  |> filter (( <= ) 100)
  |> length

let day20 input =
  let grid, s, _ = parse input in
  let path, dist = get_path grid s in
  ( string_of_int @@ solve ~cheat:2 grid dist path,
    string_of_int @@ solve ~cheat:20 grid dist path )
