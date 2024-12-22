open Utils

let parse input =
  Iter.(String.split_on_char '\n' input |> of_list |> filter ((<>) "") |> map int_of_string |> to_list)

let step s =
  Int.(
    let a = logand 0xffffff @@ logxor s (shift_left s 6) in
    let b = logand 0xffffff @@ logxor a (shift_right a 5) in
    let c = logand 0xffffff @@ logxor b (shift_left b 11) in
    c
  )

let sequence (a, b, c, d) =
  let (a, b, c, d) = Int32.(of_int a, of_int b, of_int c, of_int d) in
  Int32.(d |> logor (shift_left c 5) |> logor (shift_left b 10) |> logor (shift_left a 15))

let change seq v =
  let v = Int32.of_int v in
  Int32.(logor (sequence seq) (shift_left v 20))

let profit_at seq c =
  let open Int32 in
  if logand (of_int 0xfffff) c = seq then 
    Some (to_int @@ shift_right c 20)
  else
    None

let changes secret =
  let a = Array.make 1997 (Int32.of_int 0) in
  let open Iter in

  iterate step secret
    |> take 2001
    |> fold_map (fun (a, b, c, d) v -> (b, c, d, v), change (b - a, c - b, d - b, v - d) v) (0, 0, 0, 0)
    |> drop 4
    |> zip_i
    |> iter (fun (i, c) -> a.(i) <- c);

  a

let profit seq changes =
  Option.value ~default: 0 @@ Array.find_map (profit_at seq) changes

let total_profit changes seq =
  let open Iter in
  of_list changes |> map (profit seq) |> sum

let part1 secrets =
  let open Iter in
  of_list secrets 
  |> map (Utils.repeat step 2000)
  |> sum

let part2 secrets =
  let open Iter in
  let r = -9 -- 9 in
  let changes = on_list (map changes) secrets in
  product (product r r) (product r r)
    |> map (fun ((a, b), (c, d)) -> print_endline @@ string_of_int a ^ "," ^ string_of_int b ^ "," ^ string_of_int c ^ "," ^ string_of_int d; sequence (a, b, c, d))
    |> map (total_profit changes)
    |> max_exn

let day22 input =
  let secrets = parse input in
  (string_of_int @@ part1 secrets, string_of_int @@ part2 secrets)
