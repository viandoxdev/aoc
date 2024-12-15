open Utils

type move = Up | Down | Left | Right
type tile = Wall | Box | Nothing | LeftBox | RightBox

module P = struct
  let parse input =
    let lines = String.split_on_char '\n' input in
    let open Iter in
    let grid_lines = on_list (take_while (( <> ) "")) lines in
    let w, h = (String.length @@ List.hd grid_lines, List.length grid_lines) in
    let grid = Array.make_matrix w h Nothing in
    let bot = ref (0, 0) in

    List.iteri
      (fun y l ->
        String.iteri
          (fun x -> function
            | '@' -> bot := (x, y)
            | '#' -> grid.(x).(y) <- Wall
            | 'O' -> grid.(x).(y) <- Box
            | _ -> ())
          l)
      grid_lines;

    let moves =
      of_list lines
      |> drop_while (( <> ) "")
      |> flat_map of_str
      |> map (function
           | '^' -> Up
           | 'v' -> Down
           | '<' -> Left
           | '>' -> Right
           | c -> failwith @@ "Unexpected move char: '" ^ String.make 1 c ^ "'")
      |> to_list
    in

    (!bot, grid, moves)
end

let is_vertical = function Up | Down -> true | _ -> false
let is_horizontal = not % is_vertical

let dir = function
  | Up -> (0, -1)
  | Down -> (0, 1)
  | Left -> (-1, 0)
  | Right -> (1, 0)

let rec push ?(commit = true) grid move ((x, y) as box) =
  let dir = dir move in
  let nx, ny = Point.add box dir in
  let apply =
    if commit then fun () ->
      match grid.(x).(y) with
      | Box ->
          grid.(x).(y) <- Nothing;
          grid.(nx).(ny) <- Box;
          true
      | LeftBox ->
          grid.(x).(y) <- Nothing;
          grid.(x + 1).(y) <- Nothing;
          grid.(nx).(ny) <- LeftBox;
          grid.(nx + 1).(ny) <- RightBox;
          true
      | _ -> false
    else fun () -> true
  in

  let recurse = push ~commit grid move in
  let recurse_pretent = push ~commit:false grid move in

  match grid.(x).(y) with
  | Box -> (
      match grid.(nx).(ny) with
      | Nothing -> apply ()
      | Wall -> false
      | Box -> recurse (nx, ny) && apply ()
      | _ -> failwith "Unreachable")
  | RightBox -> recurse (x - 1, y)
  | LeftBox -> (
      match (grid.(nx).(ny), grid.(nx + 1).(ny)) with
      | Nothing, Nothing -> apply ()
      | Nothing, LeftBox when move = Left -> apply ()
      | RightBox, Nothing when move = Right -> apply ()
      | Wall, _ when is_vertical move -> false
      | _, Wall when is_vertical move -> false
      | _, Wall when move = Right -> false
      | Wall, _ when move = Left -> false
      | Nothing, LeftBox -> recurse (nx + 1, ny) && apply ()
      | RightBox, Nothing -> recurse (nx - 1, ny) && apply ()
      | LeftBox, RightBox -> recurse (nx, ny) && apply ()
      | RightBox, LeftBox when move = Right -> recurse (nx + 1, ny) && apply ()
      | RightBox, LeftBox when move = Left -> recurse (nx - 1, ny) && apply ()
      | RightBox, LeftBox ->
          recurse_pretent (nx - 1, ny)
          && recurse_pretent (nx + 1, ny)
          && recurse (nx - 1, ny)
          && recurse (nx + 1, ny)
          && apply ()
      | _, _ -> failwith "Unreachable")
  | _ -> failwith "Can only push Box, LeftBox or RightBox"

let move_bot grid bot move =
  let dir = dir move in
  let ((nx, ny) as nbot) = Point.add bot dir in
  match grid.(nx).(ny) with
  | Nothing -> nbot
  | Wall -> bot
  | _ -> if push grid move nbot then nbot else bot

let widen_grid grid (bx, by) =
  let w, h = grid_dim grid in
  let wide = Array.make_matrix (w * 2) h Nothing in
  for x = 0 to w - 1 do
    for y = 0 to h - 1 do
      match grid.(x).(y) with
      | Wall ->
          wide.(2 * x).(y) <- Wall;
          wide.((2 * x) + 1).(y) <- Wall
      | Box ->
          wide.(2 * x).(y) <- LeftBox;
          wide.((2 * x) + 1).(y) <- RightBox
      | _ -> ()
    done
  done;

  (wide, (bx * 2, by))

let solve grid bot moves =
  let open Iter in
  let w, h = grid_dim grid in
  ignore @@ List.fold_left (move_bot grid) bot moves;

  product (0 -- (w - 1)) (0 -- (h - 1))
  |> map (fun (x, y) ->
         match grid.(x).(y) with LeftBox | Box -> (y * 100) + x | _ -> 0)
  |> sum

let day15 input =
  let bot, grid, moves = P.parse input in
  let wide_grid, wide_bot = widen_grid grid bot in
  ( string_of_int @@ solve grid bot moves,
    string_of_int @@ solve wide_grid wide_bot moves )
