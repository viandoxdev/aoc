open Utils

type stripe = White | Blue | Black | Red | Green

let stripe_of_char = function
  | 'w' -> White
  | 'u' -> Blue
  | 'b' -> Black
  | 'r' -> Red
  | 'g' -> Green
  | _ -> failwith "No such stripe color"

let stripes_of_string str = Iter.(of_str str |> map stripe_of_char |> to_list)

let int_of_stripe = function
  | White -> 0
  | Blue -> 1
  | Black -> 2
  | Red -> 3
  | Green -> 4

let stripe_count = 5

module P = struct
  open Angstrom

  let is_stripe = function 'w' | 'u' | 'b' | 'r' | 'g' -> true | _ -> false
  let pattern = take_while1 is_stripe >>| stripes_of_string

  let parse =
    Result.get_ok
    % parse_string ~consume:All
        (both
           (sep_by (string ", ") pattern <* string "\n\n")
           (many (pattern <* char '\n')))
end

module StripeTree = struct
  type t = Leaf | Branch of t option array | LeafBranch of t option array

  let arr_get a s = a.(int_of_stripe s)
  let arr_empty () = Array.make stripe_count None

  let arr_with a s v =
    a.(int_of_stripe s) <- Some v;
    a

  let arr_of = arr_with (arr_empty ())
  let empty () = Branch (Array.make stripe_count None)

  let rec add p t =
    match (p, t) with
    | [], Branch a -> LeafBranch a
    | [], t -> t
    | x :: xs, Leaf -> LeafBranch (arr_of x @@ add xs @@ empty ())
    | x :: xs, Branch a -> (
        match arr_get a x with
        | None -> Branch (arr_with a x @@ add xs @@ empty ())
        | Some b -> Branch (arr_with a x @@ add xs b))
    | x :: xs, LeafBranch a -> (
        match arr_get a x with
        | None -> LeafBranch (arr_with a x @@ add xs @@ empty ())
        | Some b -> LeafBranch (arr_with a x @@ add xs b))

  module Pattern = struct
    type t = stripe list

    let equal = ( = )

    let hash =
      List.fold_left (fun a x -> Int.shift_left a 1 + int_of_stripe x) 0
  end

  module Memo = Hashtbl.Make (Pattern)

  let count t =
    let memo = Memo.create 2048 in
    fun p ->
      let rec aux ~full b t p =
        let cached = if b then Memo.find_opt memo p else None in
        match cached with
        | Some c -> c
        | None ->
            let res =
              match (t, p) with
              | Leaf, [] -> 1
              | Leaf, r -> aux ~full true full r
              | Branch _, [] -> 0
              | Branch a, x :: xs -> (
                  match arr_get a x with
                  | None -> 0
                  | Some t -> aux ~full false t xs)
              | LeafBranch _, [] -> 1
              | LeafBranch a, x :: xs -> (
                  aux ~full true full (x :: xs)
                  +
                  match arr_get a x with
                  | None -> 0
                  | Some t -> aux ~full false t xs)
            in
            if b then Memo.add memo p res;
            res
      in
      aux ~full:t true t p
end

let day19 input =
  let patterns, designs = P.parse input in
  let tree = List.fold_right StripeTree.add patterns (StripeTree.empty ()) in
  let open Iter in
  let results = on_list (map @@ StripeTree.count tree) designs in

  ( string_of_int (of_list results |> filter (( > ) 0) |> length),
    string_of_int (of_list results |> sum) )
