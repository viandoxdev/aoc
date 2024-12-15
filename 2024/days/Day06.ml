open Utils

type tile = Empty | Obstacle

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

let rec walk grid visited (x, y) (dx, dy) =
  let w, h = grid_dim grid in
  if x < 0 || y < 0 || x >= w || y >= h then Some visited
  else if List.mem (dx, dy) visited.(x).(y) then None
  else if grid.(x).(y) = Obstacle then
    walk grid visited (x - dx - dy, y - dy + dx) (-dy, dx)
  else (
    visited.(x).(y) <- (dx, dy) :: visited.(x).(y);
    walk grid visited (x + dx, y + dy) (dx, dy))

let part1 visited =
  Iter.(
    of_array visited
    |> map (fun col -> of_array col |> filter (fun c -> c != []) |> length)
    |> sum)

let part2 grid (gx, gy) visited =
  let w, h = grid_dim grid in
  let buf = Array.make_matrix w h [] in
  Iter.(
    of_array visited
    |> mapi (fun x col ->
           of_array col
           |> zip_i
           |> filter (fun (y, e) -> e != [] && not (x = gx && y = gy))
           |> filter (fun (y, _) ->
                  grid.(x).(y) <- Obstacle;

                  for x = 0 to (w - 1) do
                    Array.fill buf.(x) 0 h []
                  done;

                  let loops =
                    Option.is_none
                    @@ walk grid buf (gx, gy) (0, -1)
                  in
                  grid.(x).(y) <- Empty;
                  loops)
           |> length)
    |> sum)

let day06 input =
  let grid, guard = P.parse input in
  let w, h = grid_dim grid in
  let visited =
    Option.get @@ walk grid (Array.make_matrix w h []) guard (0, -1)
  in
  (string_of_int @@ part1 visited, string_of_int @@ part2 grid guard visited)
