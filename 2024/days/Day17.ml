open Utils

module P = struct
  open Angstrom

  let reg = string "Register " *> any_char *> string ": " *> digits <* char '\n'
  let code = string "Program: " *> sep_by (char ',') digits <* char '\n'
  let regs = both reg (both reg reg) >>| fun (a, (b, c)) -> (a, b, c)

  let parse =
    Result.get_ok % parse_string ~consume:All (both (regs <* char '\n') code)
end

type ins =
  | BXorLit of int
  | BStore of int
  | BXorC
  | ADiv of int
  | BDiv of int
  | CDiv of int

let ins_of_ints = function
  | 0, o -> ADiv o
  | 1, o -> BXorLit o
  | 2, o -> BStore o
  | 4, _ -> BXorC
  | 6, o -> BDiv o
  | 7, o -> CDiv o
  | _ -> failwith @@ "No such instruction"

let combo (a, b, c) = function 4 -> a | 5 -> b | 6 -> c | o -> o

let run_ins (a, b, c) =
  let combo = combo (a, b, c) in
  let open Int in
  function
  | ADiv o -> (shift_right a (combo o), b, c)
  | BDiv o -> (a, shift_right a (combo o), c)
  | CDiv o -> (a, b, shift_right a (combo o))
  | BXorLit l -> (a, logxor b l, c)
  | BXorC -> (a, logxor b c, c)
  | BStore o -> (a, logand 0b111 (combo o), c)

let part1 (a, b, c) code oc =
  let open Int in
  let rec aux (out, a, b, c) =
    let a, b, c = List.fold_left run_ins (a, b, c) code in
    let out = (logand 0b111 @@ combo (a, b, c) oc) :: out in
    if a = 0 then List.rev out else aux (out, a, b, c)
  in

  aux ([], a, b, c)

let part2 code oc bytecode =
  let open Iter in
  let open Int in
  let check e a =
    e = logand 0b111 @@ combo (List.fold_left run_ins (a, 0, 0) code) oc
  in
  List.fold_right
    (fun e a ->
      product (of_list a) (0 -- 7)
      |> map (fun (a, d) -> shift_left a 3 + d)
      |> filter (check e)
      |> to_list)
    bytecode [ 0 ]
  |> of_list |> min_exn

let day17 input =
  let open Iter in
  let (a, b, c), bytecode = P.parse input in

  let ops = pairs bytecode in
  let _, oc = List.nth ops (List.length ops - 2) in
  let code =
    of_list ops |> take (List.length ops - 2) |> map ins_of_ints |> to_list
  in

  let part1 = part1 (a, b, c) code oc in
  let part2 = part2 code oc bytecode in

  ( of_list part1 |> map string_of_int |> intersperse "," |> concat_str,
    string_of_int @@ part2 )
