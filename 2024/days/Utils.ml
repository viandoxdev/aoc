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
