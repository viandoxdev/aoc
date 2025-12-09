open Utils

let parse input =
    let input = trim_end_nl input in
    String.split_on_char '\n' input
        |> List.map (string_split_once "," %> tmap2 int_of_string)
        |> Array.of_list

let solve_part1 points =
    let n = Array.length points in
    range_pairs 0 (n - 1) 
        |> List.map (fun (a, b) -> points.(a), points.(b))
        |> List.map (fun ((x1, y1), (x2, y2)) -> (1 + abs (x1 - x2)) * (1 + abs (y1 - y2)))
        |> List.fold_left (max) 0

let test_points points =
    let n = Array.length points in
    range 0 (n-1) |> List.filter_map (fun i ->
        let px, py = points.(if i = 0 then n - 1 else i - 1) in
        let nx, ny = points.(if i = n - 1 then 0 else i + 1) in
        let cx, cy = points.(i) in

        if nx > cx && cy > py then Some (cx + 1, cy - 1)
        else if px > cx && ny > cy then Some (cx + 1, cy + 1)
        else if cx > nx && py > cy then Some (cx - 1, cy + 1)
        else if cx > px && cy > ny then Some (cx - 1, cy - 1)
        else None)

let solve_part2 points =
    let n = Array.length points in
    let tests = test_points points in

    let rects = range_pairs 0 (n - 1) 
        |> List.map (fun (a, b) -> points.(a), points.(b))
        |> List.map (fun ((x1, y1), (x2, y2)) -> 
            (1 + abs (x1 - x2)) * (1 + abs (y1 - y2)), 
            (min x1 x2, min y1 y2), 
            (max x1 x2, max y1 y2))
        |> List.sort (fun (a, _, _) (b, _, _) -> Int.compare b a) in

    List.(find (fun (_, (x1, y1), (x2, y2)) -> 
        not (range 0 (n - 1) 
            |> map (fun k -> (k, if k = n - 1 then 0 else k + 1))
            |> exists (fun (a, b) -> 
                let (x3, y3), (x4, y4) = points.(a), points.(b) in
                let x3, y3, x4, y4 = min x3 x4, min y3 y4, max x3 x4, max y3 y4 in
                x3 < x2 && x1 < x4 && y3 < y2 && y1 < y4))
        && List.for_all (fun (x, y) -> x < x1 || x > x2 || y < y1 || y > y2) tests
    ) rects |> fst3)

let day09 input =
    let points = parse input in
    let part1 = solve_part1 points in
    let part2 = solve_part2 points in
    (string_of_int part1, string_of_int part2)
