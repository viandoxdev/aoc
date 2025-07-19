open Utils

type tile = Empty | Obstacle

type dir = Up | Left | Right | Down
type dir_set = int

let set_empty = 0
let dir_right = function
  | Up -> Right
  | Down -> Left
  | Left -> Up
  | Right -> Down
let dir_bit = function
  | Up -> 0b0001
  | Down -> 0b0010
  | Left -> 0b00100
  | Right -> 0b1000
let dir_off = function
  | Up -> (0, -1)
  | Down -> (0, 1)
  | Left -> (-1, 0)
  | Right -> (1, 0)

let dir_mem dir set = Int.logand set (dir_bit dir) > 0
let dir_add dir set = Int.logor set (dir_bit dir)

module P = struct
  let parse input =
    let lines = String.split_on_char '\n' input in
    let w = String.length @@ List.hd lines in
    let h = List.length lines - 1 in
    let grid = Array.make_matrix w h Empty in
    List.iteri
      (fun y line ->
        String.iteri (fun x c -> if c = '#' then grid.(x).(y) <- Obstacle) line)
      lines;
    ( grid,
      Option.get
      @@ Iter.(
           of_list lines
           |> find_mapi (fun y line ->
                  String.index_opt line '^' |> Option.map (fun x -> (x, y)))) )
end

let rec walk grid visited (x, y) dir =
  let w, h = grid_dim grid in
  let dx, dy = dir_off dir in
  if x < 0 || y < 0 || x >= w || y >= h then Some visited
  else if dir_mem dir visited.(x).(y) then None
  else if grid.(x).(y) = Obstacle then
    walk grid visited (x - dx - dy, y - dy + dx) (dir_right dir)
  else (
    visited.(x).(y) <- dir_add dir visited.(x).(y);
    walk grid visited (x + dx, y + dy) dir)

let part1 visited =
  Iter.(
    of_array visited
    |> map (fun col -> of_array col |> filter (fun c -> c != set_empty) |> length)
    |> sum)

let part2 grid (gx, gy) visited =
  let w, h = grid_dim grid in
  let buf = Array.make_matrix w h set_empty in
  Iter.(
    of_array visited
    |> mapi (fun x col ->
           of_array col |> zip_i
           |> filter (fun (y, e) -> e != set_empty && not (x = gx && y = gy))
           |> filter (fun (y, _) ->
                  grid.(x).(y) <- Obstacle;

                  for x = 0 to w - 1 do
                    Array.fill buf.(x) 0 h set_empty
                  done;

                  let loops =
                    Option.is_none @@ walk grid buf (gx, gy) Up
                  in
                  grid.(x).(y) <- Empty;
                  loops)
           |> length)
    |> sum)

let day06 input =
  let grid, guard = P.parse input in
  let w, h = grid_dim grid in
  let visited =
    Option.get @@ walk grid (Array.make_matrix w h set_empty) guard Up
  in
  (string_of_int @@ part1 visited, string_of_int @@ part2 grid guard visited)
