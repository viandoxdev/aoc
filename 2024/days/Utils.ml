open Angstrom

let is_digit = function '0' .. '9' -> true | _ -> false
let is_whitespace = function ' ' -> true | _ -> false
let digits = take_while1 is_digit >>| fun x -> int_of_string x
let whitespace = take_while is_whitespace

let rec head_nth n xs =
  if n > 0 then List.hd xs :: head_nth (n - 1) (List.tl xs) else []

let rec sliding_window s xs =
  if List.length xs <= s then [ xs ]
  else head_nth s xs :: sliding_window s (List.tl xs)

let sliding_pairs xs =
  let _, pairs =
    List.fold_left
      (fun (last, acc) c -> (c, (last, c) :: acc))
      (List.hd xs, [])
      (List.tl xs)
  in
  List.rev pairs

let reduce f xs = List.fold_left f (List.hd xs) (List.tl xs)
let sum xs = reduce ( + ) xs
let remove_nth n = List.filteri (fun i _ -> i != n)

let print_int_list xs =
  print_endline @@ "[" ^ (String.concat "; " @@ List.map string_of_int xs) ^ "]"

let id x = x

let grid_dim grid =
  let w = Array.length grid in
  let h = Array.length grid.(0) in
  (w, h)

let int_of_bool = function true -> 1 | false -> 0

let pows =
  Array.of_list @@ Iter.(iterate (fun x -> x * 10) 1 |> take 20 |> to_list)

let int_width = Ocaml_intrinsics.Int.count_leading_zeros 0

let log10 n =
  let b = int_width - 1 - Ocaml_intrinsics.Int.count_leading_zeros n in
  let a = b * 77 / 256 in
  1 + a + int_of_bool (n >= pows.(a + 1))

module Point = struct
  type int_line = { p : int; q : int; x1 : int; y1 : int }

  let line_of_points (x1, y1) (x2, y2) =
    if x1 = x2 then { p = 1; q = 0; x1; y1 }
    else
      let m = Q.of_ints (y2 - y1) (x2 - x1) in
      let p, q = (Z.to_int m.num, Z.to_int m.den) in
      { p; q; x1; y1 }

  let line_get_point { p; q; x1; y1 } k =
    let x = (k * q) + x1 in
    let y = if q != 0 then (((p * x) - (p * x1)) / q) + y1 else y1 + k in
    (x, y)

  let sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
  let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  let scale k (x, y) = (k * x, k * y)

  let string_of_point (x, y) =
    "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

  let sq_dist (x1, y1) (x2, y2) =
    let dx, dy = (x2 - x1, y2 - y1) in
    (dx * dx) + (dy * dy)
end
