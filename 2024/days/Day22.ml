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

let map_quad f (a, b, c, d) =
  (f a, f b, f c, f d)

module Sequence = struct
  type t = int32

  let equal = Int32.equal
  let hash a = Int32.to_int a
  let of_tuple t =
    let (a, b, c, d) = map_quad (Int32.of_int % ((+) 9)) t in
    Int32.(d |> logor (shift_left c 5) |> logor (shift_left b 10) |> logor (shift_left a 15))
  let to_tuple s =
    let open Int32 in
    let mask = of_int 0x1f in
    map_quad (((+) (-9)) % to_int % (logand mask) % (shift_right s)) (15,10,5,0)
end

module Table = Hashtbl.Make(Sequence)

let change_table ?(n=2001) secret =
  let m = Table.create 2048 in
  let open Iter in

  iterate step secret
    |> take n
    |> map (fun x -> x mod 10)
    |> fold_map (fun (a, b, c, d) v -> (b, c, d, v), (Sequence.of_tuple (b - a, c - b, d - c, v - d), v)) (0, 0, 0, 0)
    |> drop 4
    |> iter (fun (s, v) -> if not @@ Table.mem m s then Table.add m s v);

  m

let profit seq table =
  Option.value ~default:0 @@ Table.find_opt table seq

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
  let changes = on_list (map change_table) secrets in
  product (product r r) (product r r)
    |> map (fun ((a, b), (c, d)) -> Sequence.of_tuple (a, b, c, d))
    |> map (total_profit changes)
    |> max_exn

let day22 input =
  let secrets = parse input in
  (string_of_int @@ part1 secrets, string_of_int @@ part2 secrets)
