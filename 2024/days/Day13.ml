open Utils

type machine = { a : int * int; b : int * int; p : int * int }

module P = struct
  open Angstrom

  let button =
    both
      (string "Button " *> any_char *> string ": X+" *> digits <* string ", Y+")
      (digits <* char '\n')

  let prize =
    both (string "Prize: X=" *> digits <* string ", Y=") (digits <* char '\n')

  let machine =
    both (both button button) prize <* many (char '\n')
    >>= fun ((a, b), p) -> return { a; b; p }

  let parse = Result.get_ok % Angstrom.parse_string ~consume:All (many machine)
end

let solve ?(delta = 0) machines =
  let open Iter in
  of_list machines
  |> map (fun { a = ax, ay; b = bx, by; p = px, py } ->
         let px, py = (px + delta, py + delta) in
         let a = ((by * px) - (bx * py)) / ((by * ax) - (bx * ay)) in
         let b = (px - (a * ax)) / bx in
         if (a * ax) + (b * bx) = px && (a * ay) + (b * by) = py then
           (a * 3) + b
         else 0)
  |> sum

let day13 input =
  let machines = P.parse input in
  ( string_of_int @@ solve machines,
    string_of_int @@ solve ~delta:10000000000000 machines )
