open Utils

module P = struct
  open Angstrom

  let rule =
    both digits (char '|' *> digits)
  let update = 
    sep_by (char ',') digits
  let rules =
    many (rule <* char '\n')
  let updates =
    many (update <* char '\n')
  let parse input =
    Result.get_ok @@ Angstrom.parse_string ~consume: All (both rules (char '\n' *> updates)) input
end

let rule_array rules =
  let arr = Array.make 99 [] in
  List.iter (fun (a, b) -> 
    arr.(a) <- b :: arr.(a)
  ) rules;
  arr

let is_ordered rarr update =
  Iter.(
    of_list update
      |> mapi (fun i n ->
          of_list update
            |> mapi (fun j m -> j >= i || not @@ List.mem m rarr.(n))
            |> for_all id
      )
      |> for_all id
  )

let reorder rarr update =
  List.(
    update
      |> map (fun e -> length @@ filter (fun d -> mem d update) rarr.(e), e)
      |> sort (fun (a, _) (b, _) -> Int.compare a b)
      |> map (fun (_, e) -> e)
  )

let middle_page update =
  let len = List.length update in
  List.nth update (len / 2)

let middle_page_sum updates =
  List.map middle_page updates
    |> sum

let day05 input =
  let (rules, updates) = P.parse input in
  let rarr = rule_array rules in
  let (ordered, unordered) = List.partition (is_ordered rarr) updates in
  let reordered = List.map (reorder rarr) unordered in
  (string_of_int @@ middle_page_sum ordered, string_of_int @@ middle_page_sum reordered)
