open Utils

(* Paper and Empty are explicitn
   Removed n is a Paper removed on the nth iteration
   This means we don't need to copy the grid *)
type tile = Paper | Removed of int | Empty

let is_paper n = function
    | Paper -> true
    | Empty -> false
    | Removed m -> n < m

let parse =
    parse_grid 
        (fun w h -> Array.make_matrix w h Empty) 
        (fun x y c g -> 
            g.(x).(y) <- if c = '@' then Paper else Empty;
            g
        )

let paper_count n g x y =
    let w, h = dim g in
    List.fold_left 
        (fun a (nx, ny) -> a + int_of_bool (is_paper n g.(nx).(ny))) 0 (inbounds_neighbours w h neighbours_8 x y)

let remove_accessible_rolls n grid =
    let w, h = dim grid in
    range_sum (fun y -> 
        range_sum (fun x -> 
            if not (is_paper n grid.(x).(y)) then 0 else
            let papers_neighbours = paper_count n grid x y in
            if papers_neighbours <= 3 then (grid.(x).(y) <- Removed (n + 1); 1) else 0)
            0 (w - 1))
        0 (h - 1) 

let solve_part1 grid =
    let grid = copy_grid grid in
    remove_accessible_rolls 0 grid

let solve_part2 grid =
    let grid = copy_grid grid in
    let rec aux n sum grid = match remove_accessible_rolls n grid with
        | 0 -> sum
        | rem -> aux (n+1) (sum + rem) grid
    in
    aux 0 0 grid

let day04 input =
    let grid = parse input in
    let part1 = solve_part1 grid in
    let part2 = solve_part2 grid in
    (string_of_int part1, string_of_int part2)
