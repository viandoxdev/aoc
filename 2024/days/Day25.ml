open Utils

let parse input =
  String.split_on_char '\n' (input ^ "\n")
  |> List.fold_left
       (fun (xs, b) l ->
         if l = "" && Option.is_some b then (Option.get b :: xs, None)
         else
           let b = Option.value ~default:0 b in
           let b =
             String.fold_left
               (fun b c -> Int.(logor (shift_left b 1) (int_of_bool (c = '#'))))
               b l
           in
           (xs, Some b))
       ([], None)
  |> fst
  |> List.partition (fun b -> Int.logand b 1 = 0)

let part1 locks keys =
  let open Iter in
  product (of_list locks) (of_list keys)
  |> filter (fun (l, k) -> Int.logand l k = 0)
  |> length

let day25 input =
  let locks, keys = parse input in
  (string_of_int @@ part1 locks keys, "")
