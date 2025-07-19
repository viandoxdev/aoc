open Utils

let parse input =
  Iter.(
    String.split_on_char '\n' input
    |> of_list
    |> filter (( <> ) "")
    |> map int_of_string |> to_list)

let step s =
  Int.(
    let a = logand 0xffffff @@ logxor s (shift_left s 6) in
    let b = logand 0xffffff @@ logxor a (shift_right a 5) in
    let c = logand 0xffffff @@ logxor b (shift_left b 11) in
    c)

module Sequence = struct
  type t = int

  let all_sequences = Iter.(0 -- ((19 * 19 * 19 * 19) - 1))

  let of_tuple t =
    let a, b, c, d = map_quad (( + ) 9) t in
    (a * 19 * 19 * 19) + (b * 19 * 19) + (c * 19) + d

  let rot t v = (t mod (19 * 19 * 19) * 19) + (v + 9)

  let to_tuple s =
    map_quad (fun x -> (s / x mod 19) - 9) (19 * 19 * 19, 19 * 19, 19, 1)
end

let profit_table secrets =
  let n = 19 * 19 * 19 * 19 in
  let profit = Array.make n 0 in
  let seen = Array.make n 0 in

  List.iteri
    (fun i secret ->
      let open Iter in
      iterate step secret |> take 2001
      |> map (fun x -> x mod 10)
      |> fold_map
           (fun (p, s) v ->
             let ns = Sequence.rot s (v - p) in
             ((v, ns), (ns, v)))
           (0, 0)
      |> drop 4
      |> iter (fun (seq, v) ->
             if seen.(seq) != i then (
               profit.(seq) <- profit.(seq) + v;
               seen.(seq) <- i)))
    secrets;

  profit

let part1 secrets =
  Iter.(of_list secrets |> map (Utils.repeat step 2000) |> sum)

let part2 = Array.fold_left max 0 % profit_table

let day22 input =
  let secrets = parse input in
  (string_of_int @@ part1 secrets, string_of_int @@ part2 secrets)
