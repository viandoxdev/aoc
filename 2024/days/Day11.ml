open Utils

let parse =
  let open Angstrom in
  Result.get_ok
  % parse_string ~consume:All (sep_by (char ' ') digits <* char '\n')

let step m =
  let open Iter in
  let memo = Hashtbl.create 2048 in

  let rec count depth stone =
    if depth >= m then 1
    else
      let count_next = count (depth + 1) in

      let cached = Hashtbl.find_opt memo (stone, depth) in
      match cached with
      | Some res -> res
      | None ->
          let res =
            match stone with
            | 0 -> count_next 1
            | s ->
                let digits = log10 s in
                if digits mod 2 = 0 then
                  let p = pows.(digits / 2) in
                  let left = s / p in
                  let right = s - (left * p) in

                  count_next left + count_next right
                else count_next (s * 2024)
          in
          Hashtbl.add memo (stone, depth) res;
          res
  in
  fun stones -> of_list stones |> map (count 0) |> sum

let part1 = step 25
let part2 = step 75

let day11 input =
  let stones = parse input in
  (string_of_int @@ part1 stones, string_of_int @@ part2 stones)
