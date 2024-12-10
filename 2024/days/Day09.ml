open Utils

let parse input =
  let open Iter in
  of_str input |> filter is_digit
  |> map (fun c -> (int_of_char c - int_of_char '0', 0))
  |> map_by_2 (fun (a, _) (b, _) -> (a, b))
  |> mapi (fun i (a, b) -> (i, a, b))
  |> to_list

let sum_range a l = (a * l) + (l * (l - 1) / 2)

let checksum files =
  let rec aux fwd bwd pos max_pos =
    match (fwd, bwd) with
    | _ when pos >= max_pos -> 0
    | (_, 0, 0) :: fwd, _ -> aux fwd bwd pos max_pos
    | (_, 0, f) :: fwd, (i, b, p) :: bwd when b > f ->
        aux ((i, f, 0) :: fwd) ((i, b - f, p) :: bwd) pos max_pos
    | (_, 0, f) :: fwd, (i, b, _) :: bwd when b <= f ->
        aux ((i, b, f - b) :: fwd) bwd pos max_pos
    | (i, b, f) :: fwd, _ ->
        let a = min (max_pos - pos) b in
        (i * sum_range pos a) + aux ((i, 0, f) :: fwd) bwd (pos + a) max_pos
    | _ -> failwith "Unreachable"
  in
  let max_pos = Iter.(of_list files |> map (fun (_, a, _) -> a) |> sum) in
  aux files (List.rev files) 0 max_pos

let id_eq i (j, _, _) = i = j

let reorder files =
  let rec aux fwd arr buf n =
    let open Iter in
    match fwd with
    | [] -> List.rev buf
    | (i, b, f) :: fwd -> (
        let e =
          if f = 0 then None
          else
            n --^ 0
            |> map (fun j -> arr.(j))
            |> keep_some
            |> find (fun e ->
                   let j, b, _ = e in
                   if b <= f && i != j then Some e else None)
        in
        match e with
        | Some (j, p, _) -> aux ((i, b, 0) :: (j, p, f - p) :: fwd) arr buf n
        | None ->
            if Option.is_some arr.(i) then (
              arr.(i) <- None;
              aux fwd arr ((i, b, f) :: buf) n)
            else
              let pi, pb, pf = List.hd buf in
              aux fwd arr ((pi, pb, pf + b + f) :: List.tl buf) n)
  in

  let arr = Array.of_list @@ List.map (fun x -> Some x) files in
  let _, res =
    List.fold_left
      (fun (pos, a) (i, b, f) -> (pos + b + f, a + (i * sum_range pos b)))
      (0, 0)
    @@ aux files arr [] (List.length files - 1)
  in
  res

let day09 input =
  let files = parse input in
  (string_of_int @@ checksum files, string_of_int @@ reorder files)
