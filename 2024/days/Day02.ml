open Angstrom
open Utils
open List

let parse_input =
  many1 (many1 (digits <* whitespace) <* char '\n') 

let parse input =
  Result.get_ok @@ Angstrom.parse_string ~consume: All parse_input input

let report_error report =
  let deltas = map (fun (p, c) -> p - c) @@ sliding_pairs report in
  filter_map (fun (i, a) -> if a > 3 then Some i else None) @@ mapi (fun i d -> (i, abs d)) deltas,
  filter_map (fun (i, a, b) -> if a != b || a == 0 || b == 0 then Some i else None) @@ mapi (fun i (a, b) -> (i, Int.compare a 0, Int.compare b 0)) @@ sliding_pairs deltas

let check_report report =
  let (deltas, monotony) = report_error report in
  deltas = [] && monotony = []

let check_report_dampened report =
  let reports = report :: List.mapi (fun i _ -> remove_nth i report) report in
  exists check_report reports

let part1 reports =
  List.length @@ List.filter check_report reports

let part2 reports =
  List.length @@ List.rev @@ List.filter check_report_dampened reports

let day02 input =
  let reports = parse input in
  (string_of_int @@ part1 reports, string_of_int @@ part2 reports)
