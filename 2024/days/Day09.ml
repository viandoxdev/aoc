open Utils

let parse input =
  let open Iter in
  snoc (of_str input) '0' |> filter is_digit
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

type space = Cons of {
  mutable next: space; 
  mutable prev : space; 
  mutable size: int; 
  mutable at: int;
} | Nil

let reorder input =
  let file_checksum id size at = id * (size * at + size * (size - 1) / 2) in
  let spaces = ref Nil in
  let open Iter in
  let (files_rev, _, _) = snoc (of_str input) '0' |> filter is_digit
  |> map (fun c -> (int_of_char c - int_of_char '0', 0))
  |> map_by_2 (fun (a, _) (b, _) -> (a, b))
  |> foldi (fun (files, last_space, at) id (file, space) -> (
    (id, file, at) :: files,
    (if space > 0 then 
      (let next_space = Cons {next = Nil ; prev = last_space ; size = space ; at = at + file} in
        (match last_space with
          | Nil -> spaces := next_space
          | Cons last -> last.next <- next_space);
        next_space)
      else last_space),
    at + file + space
  )) ([], !spaces, 0) in
  
  of_list files_rev |> fold (fun total (id, fsize, fat) -> (
    let rec aux = function
      | Cons {next ; size ; _} when size < fsize -> aux next
      | Cons {at ; _} when at >= fat -> aux Nil
      | Cons {next ; prev ; size ; at} when size = fsize -> (
        (match prev with 
          | Nil -> spaces := next
          | Cons cell -> cell.next <- next);
        (match next with
          | Nil -> ()
          | Cons cell -> cell.prev <- prev);
        total + file_checksum id fsize at)
      | Cons space -> (
        let at = space.at in
        space.at <- at + fsize;
        space.size <- space.size - fsize;
        total + file_checksum id fsize at)
      | Nil -> (
        total + file_checksum id fsize fat
      )
    in aux !spaces
  )) 0

let day09 input =
  let files = parse input in
  (string_of_int @@ checksum files, string_of_int @@ reorder input)
