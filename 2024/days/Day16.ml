open Utils

type tile = Wall | Nothing
type direction = North | West | South | East

let directions = [ North; West; South; East ]

let right = function
  | North -> East
  | East -> South
  | South -> West
  | West -> North

let left = function
  | West -> South
  | South -> East
  | East -> North
  | North -> West

let int_of_dir = function North -> 0 | West -> 1 | South -> 2 | East -> 3

let vec = function
  | North -> (0, -1)
  | West -> (-1, 0)
  | South -> (0, 1)
  | East -> (1, 0)

let parse input =
  let lines = String.split_on_char '\n' input in
  let w, h = (String.length @@ List.hd lines, List.length lines - 1) in
  let grid = Array.make_matrix w h Nothing in
  let start_tile = ref (0, 0) in
  let end_tile = ref (0, 0) in
  List.iteri
    (fun y l ->
      String.iteri
        (fun x -> function
          | '#' -> grid.(x).(y) <- Wall
          | 'S' -> start_tile := (x, y)
          | 'E' -> end_tile := (x, y)
          | _ -> ())
        l)
    lines;
  (grid, !start_tile, East, !end_tile)

module Vertex = struct
  type t = { c : int; d : direction; p : int * int }

  let leq { c = a; _ } { c = b; _ } = a <= b
end

module Prev = struct
  type t = int * int * direction

  let compare a b = compare a b
end

module VertexHeap = CCHeap.Make (Vertex)
module PrevSet = Set.Make (Prev)

let dijkstra grid (x, y) d =
  let open Iter in
  let w, h = grid_dim grid in
  let dist = Array.init 4 (fun _ -> Array.make_matrix w h Int.max_int) in
  let prev = Array.init 4 (fun _ -> Array.make_matrix w h []) in
  let q = VertexHeap.(empty |> insert { c = 0; d; p = (x, y) }) in

  dist.(int_of_dir d).(x).(y) <- 0;

  let rec aux q =
    match VertexHeap.take q with
    | Some (q, { c; d; p = x, y }) ->
        of_list [ (1, d); (1001, left d); (1001, right d) ]
        |> map (fun (nc, nd) -> (c + nc, nd, Point.add (x, y) (vec nd)))
        |> filter (fun (nc, nd, (nx, ny)) ->
               grid.(nx).(ny) = Nothing && nc <= dist.(int_of_dir nd).(nx).(ny))
        |> fold
             (fun q (nc, nd, (nx, ny)) ->
               let i = int_of_dir nd in
               if nc < dist.(i).(nx).(ny) then prev.(i).(nx).(ny) <- [];
               prev.(i).(nx).(ny) <- (x, y, d) :: prev.(i).(nx).(ny);
               dist.(i).(nx).(ny) <- nc;
               VertexHeap.(add q { c = nc; d = nd; p = (nx, ny) }))
             q
        |> aux
    | _ -> (dist, prev)
  in

  aux q

let count_seats grid prev dist min_dist (ex, ey) =
  let open Iter in
  let w, h = grid_dim grid in
  let is_best = Array.make_matrix w h false in

  let rec bfs s =
    if not @@ PrevSet.is_empty s then (
      PrevSet.iter (fun (x, y, _) -> is_best.(x).(y) <- true) s;
      bfs
        (of_set (module PrevSet) s
        |> flat_map_l (fun (x, y, d) -> prev.(int_of_dir d).(x).(y))
        |> to_set (module PrevSet)))
  in

  of_list directions
  |> filter (fun d -> dist.(int_of_dir d).(ex).(ey) = min_dist)
  |> map (fun d -> (ex, ey, d))
  |> to_set (module PrevSet)
  |> bfs;

  of_array is_best |> map (fun c -> of_array c |> filter id |> length) |> sum

let day16 input =
  let grid, s, dir, (ex, ey) = parse input in
  let dist, prev = dijkstra grid s dir in
  let min_dist =
    Iter.(
      of_list directions
      |> map (fun d -> dist.(int_of_dir d).(ex).(ey))
      |> min_exn)
  in
  ( string_of_int @@ min_dist,
    string_of_int @@ count_seats grid prev dist min_dist (ex, ey) )
