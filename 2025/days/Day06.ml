open Utils

let parse input =
    let (ops, numbers) = String.split_on_char '\n' input
        |> List.rev 
        |> function 
            | [] -> failwith "bad input"
            | x::xs -> x ^ " ", List.rev xs |> List.map (fun s -> s ^ " ")
    in
    let (s1, s2, _, _, _) = fold_range (fun (s1, s2, op, vers, hors) i -> 
        let op = match op with 
            | Some op -> Some op
            | None -> (match ops.[i] with
                | '*' -> Some (List.fold_left ( * ) 1)
                | '+' -> Some (List.fold_left (+) 0)
                | _ -> None) in
        let n_vers = List.fold_left (fun a -> function
            | '0'..'9' as c -> a * 10 + (int_of_char c - int_of_char '0')
            | _ -> a) 0 (List.map (fun l -> l.[i]) numbers) in
        let hors = List.map2 (fun a l -> match l.[i] with
            | '0'..'9' as c -> a * 10 + (int_of_char c - int_of_char '0')
            | _ -> a) hors numbers in
        if List.for_all (fun l -> l.[i] = ' ') numbers then
            let op = Option.get op in
            (s1 + op hors, s2 + op vers, None, [], List.map (fun _ -> 0) numbers)
        else
            (s1, s2, op, n_vers :: vers, hors)

    ) (0, 0, None, [], List.map (fun _ -> 0) numbers) 0 (String.length ops - 1)
    in
    s1, s2

let day06 input =
    let input = trim_end_nl input in
    let part1, part2 = parse input in
    (string_of_int part1, string_of_int part2)


