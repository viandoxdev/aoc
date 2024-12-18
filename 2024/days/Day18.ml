open Utils

let parse =
  let open Angstrom in
  Result.get_ok
  % parse_string ~consume:All
      (many @@ both (digits <* char ',') (digits <* char '\n'))

let w, h = (71, 71)

module Vertex = struct
  type t = { d : int; p : int * int }

  let leq { d = a; _ } { d = b; _ } = a <= b
end

module VertexHeap = CCHeap.Make (Vertex)

let dijsktra grid ((sx, sy) as s) =
  let open Iter in
  let dist = Array.make_matrix w h Int.max_int in
  let prev = Array.make_matrix w h None in
  let q = VertexHeap.(empty |> insert { d = 0; p = s }) in
  dist.(sx).(sy) <- 0;

  let rec aux q =
    match VertexHeap.take q with
    | Some (q, { d; p }) ->
        of_list [ (1, 0); (0, 1); (-1, 0); (0, -1) ]
        |> map (Point.add p)
        |> filter (fun (x, y) ->
               x >= 0 && y >= 0 && x < w && y < h
               && grid.(x).(y)
               && d + 1 < dist.(x).(y))
        |> fold
             (fun q (x, y) ->
               dist.(x).(y) <- d + 1;
               prev.(x).(y) <- Some p;
               VertexHeap.add q { d = d + 1; p = (x, y) })
             q
        |> aux
    | None -> (dist, prev)
  in

  aux q

let path_and_dist grid =
  let d, p = dijsktra grid (0, 0) in
  let rec walk (x, y) buf =
    match p.(x).(y) with
    | Some p -> walk p ((x, y) :: buf)
    | None -> (x, y) :: buf
  in
  (d.(w - 1).(h - 1), walk (w - 1, h - 1) [])

let day18 input =
  let bytes = parse input in
  let open Iter in
  let grid = Array.make_matrix w h true in

  of_list bytes |> take 1024 |> iter (fun (x, y) -> grid.(x).(y) <- false);

  let part1, path = path_and_dist grid in

  let part2, _ =
    of_list bytes |> drop 1024
    |> fold_while
         (fun (r, last_path) (x, y) ->
           grid.(x).(y) <- false;
           if List.mem (x, y) last_path then
             let d, path = path_and_dist grid in
             if d < Int.max_int then ((r, path), `Continue)
             else (((x, y), []), `Stop)
           else ((r, last_path), `Continue))
         ((0, 0), path)
  in

  ( string_of_int @@ part1,
    string_of_int (fst part2) ^ "," ^ string_of_int (snd part2) )
