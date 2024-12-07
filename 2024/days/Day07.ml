open Utils
open Lazy

module P = struct
  open Angstrom

  let equation = both (digits <* char ':') @@ many (char ' ' *> digits)

  let parse input =
    Result.get_ok
    @@ parse_string ~consume:All (many (equation <* char '\n')) input
    |> List.map (fun (e, l) -> (e, List.rev l))
end

module S = struct
  type solvability = ConcatSolvable | Solvable | Unsolvable

  let ( ||> ) a b =
    match force a with
    | Solvable -> Solvable
    | Unsolvable -> force b
    | ConcatSolvable -> if force b == Solvable then Solvable else ConcatSolvable

  let ( &&> ) a b = match a with Unsolvable -> lazy Unsolvable | _ -> b
  let of_bool = function true -> Solvable | false -> Unsolvable

  let force_concat s =
    match s with Unsolvable -> Unsolvable | _ -> ConcatSolvable

  let rec solvable = function
    | e, _ when e < 0 -> Unsolvable
    | _, [] -> Solvable
    | e, [ a ] -> of_bool (e = a)
    | e, a :: xs ->
        let sol_sum = lazy (solvable (e - a, xs)) in
        let sol_prod = of_bool (e mod a = 0) &&> lazy (solvable (e / a, xs)) in
        let sol_concat =
          lazy
            (let d = pows.(log10 a) in
             force_concat
             @@ force
                  (of_bool ((e - a) mod d = 0)
                  &&> lazy (solvable ((e - a) / d, xs))))
        in
        sol_sum ||> lazy (sol_prod ||> sol_concat)
end

let day07 input =
  let equations = P.parse input in
  let part1, part2 =
    List.fold_left
      (fun (a, b) (e, l) ->
        match S.solvable (e, l) with
        | S.Unsolvable -> (a, b)
        | S.Solvable -> (a + e, b + e)
        | S.ConcatSolvable -> (a, b + e))
      (0, 0) equations
  in
  (string_of_int @@ part1, string_of_int @@ part2)
