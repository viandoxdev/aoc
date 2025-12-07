open Utils

type tile = Empty | Splitter | Start

let parse input =
    let (a, s, p) = parse_grid 
        (fun w h -> (Array.make_matrix w h Empty, [], None))
        (fun x y c (g, s, p) -> 
            match c with
                | '^' -> (g.(x).(y) <- Splitter; (g, (x, y) :: s, p))
                | 'S' -> (g.(x).(y) <- Start; (g, s, Some (x, y)))
                | _ -> (g, s, p)) 
        input in
    (a, List.rev s, Option.get p)

let can_split x y grid =
    let get = grid_safe_access grid Empty in
    let rec aux = function
        | -1 -> false
        | y when get x y = Start -> true
        | y when get x y = Splitter -> false
        | y -> get (x - 1) y = Splitter || get (x + 1) y = Splitter || aux (y - 1)
    in aux (y - 1)

let solve_part1 grid =
    List.filter (fun (x, y) -> can_split x y grid) %> List.length

let parents_pos grid pos x y =
    let getg = grid_safe_access grid Empty  in
    let getp = grid_safe_access pos 0 in 

    let rec aux = function
        | -1 -> 0
        | y when getg x y = Splitter -> 0
        | y -> getp x y + aux (y - 1)
    in aux (y - 1)

let solve_part2 grid splitters (sx, sy) =
    let w, h = dim grid in
    let pos = Array.make_matrix w h 0 in
    let wp = grid_safe_write pos in
    let gp = grid_safe_access pos 0 in
    wp sx sy 1;

    List.iter (fun (x, y) -> 
        let par_pos = parents_pos grid pos x y in
        wp (x - 1) y (gp (x - 1) y + par_pos);
        wp (x + 1) y (gp (x + 1) y + par_pos);
    ) splitters;

    sum_range 0 (w - 1) (fun x -> parents_pos grid pos x (h - 1))

let day07 input =
    let grid, splitters, start = parse input in
    let part1 = solve_part1 grid splitters in
    let part2 = solve_part2 grid splitters start in
    (string_of_int part1, string_of_int part2)
