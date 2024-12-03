open Angstrom
open Utils
open List

let parse_input = many1 (many1 (digits <* whitespace) <* char '\n')

let parse input =
  Result.get_ok @@ Angstrom.parse_string ~consume:All parse_input input

let check_report report =
  let deltas = map (fun (p, c) -> p - c) @@ sliding_pairs report in
  for_all (fun d -> d != 0 && abs d <= 3) deltas
  && for_all (fun (a, b) -> a = b)
     @@ map (fun (a, b) -> (Int.compare a 0, Int.compare b 0))
     @@ sliding_pairs deltas

let check_report_dampened report =
  let reports = report :: mapi (fun i _ -> remove_nth i report) report in
  exists check_report reports

let part1 reports = length @@ filter check_report reports
let part2 reports = length @@ filter check_report_dampened reports

let day02 input =
  let reports = parse input in
  (string_of_int @@ part1 reports, string_of_int @@ part2 reports)
