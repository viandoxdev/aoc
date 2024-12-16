open Lwt
open List
open Cohttp_lwt_unix
open Days

type aoc_ctx = { session : string }

type day_result = {
  number : int;
  part1 : string;
  part2 : string;
  duration : float;
}

let session () =
  let ic = open_in "../session" in
  try input_line ic
  with e ->
    close_in_noerr ic;
    raise e

let fetch_input ctx day =
  let uri =
    Uri.of_string @@ "https://adventofcode.com/2024/day/" ^ string_of_int day
    ^ "/input"
  in
  let headers = Cohttp.Header.init_with "Cookie" ("session=" ^ ctx.session) in
  Client.get ~headers uri >>= fun (_, body) -> body |> Cohttp_lwt.Body.to_string

let input ctx day =
  if not (Sys.file_exists "inputs") then Sys.mkdir "inputs" 0o777;
  let path = "inputs/" ^ string_of_int day in
  if Sys.file_exists path then (
    let ic = open_in path in
    try In_channel.input_all ic
    with e ->
      close_in_noerr ic;
      raise e)
  else
    let input = Lwt_main.run @@ fetch_input ctx day in
    let oc = open_out path in
    output_string oc input;
    input

let days =
  [
    (1, Day01.day01);
    (2, Day02.day02);
    (3, Day03.day03);
    (4, Day04.day04);
    (5, Day05.day05);
    (6, Day06.day06);
    (7, Day07.day07);
    (8, Day08.day08);
    (9, Day09.day09);
    (10, Day10.day10);
    (11, Day11.day11);
    (12, Day12.day12);
    (13, Day13.day13);
    (14, Day14.day14);
    (15, Day15.day15);
    (16, Day16.day16);
  ]

let run_day ctx (day, fn) =
  let inp = input ctx day in
  let start_time = Sys.time () in
  let part1, part2 = fn inp in
  { number = day; part1; part2; duration = Sys.time () -. start_time }

let format_duration =
  let strfloat f =
    let res = string_of_float f in
    if String.ends_with ~suffix:"." res then
      String.sub res 0 (String.length res - 1)
    else res
  in
  function
  | t when t < 0.00001 ->
      (strfloat @@ ((Float.round @@ (t *. 100000000.0)) /. 100.0)) ^ " µs"
  | t when t < 0.00010 ->
      (strfloat @@ ((Float.round @@ (t *. 10000000.0)) /. 10.00)) ^ " µs"
  | t when t < 0.00100 ->
      (strfloat @@ ((Float.round @@ (t *. 1000000.0)) /. 1.000)) ^ " µs"
  | t when t < 0.01000 ->
      (strfloat @@ ((Float.round @@ (t *. 100000.0)) /. 100.0)) ^ " ms"
  | t when t < 0.10000 ->
      (strfloat @@ ((Float.round @@ (t *. 10000.0)) /. 10.00)) ^ " ms"
  | t when t < 1.00000 ->
      (strfloat @@ ((Float.round @@ (t *. 1000.0)) /. 1.000)) ^ " ms"
  | t when t < 10.0000 ->
      (strfloat @@ ((Float.round @@ (t *. 100.0)) /. 100.0)) ^ " s"
  | t when t < 100.000 ->
      (strfloat @@ ((Float.round @@ (t *. 10.0)) /. 10.00)) ^ " s"
  | t -> (string_of_float @@ Float.round t) ^ " s"

let format_colored_duration m t =
  match (m, t) with
  | m, t when t < 0.8 *. m -> "\x1b[92m" ^ format_duration t ^ "\x1b[0m"
  | m, t when t <= m -> "\x1b[93m" ^ format_duration t ^ "\x1b[0m"
  | _, t -> "\x1b[91m" ^ format_duration t ^ "\x1b[0m"

let display_results res total_duration =
  print_endline "AOC 2024:";
  print_endline "";
  let runtime_sum = fold_left ( +. ) 0.0 @@ map (fun d -> d.duration) res in

  List.iter
    (fun day ->
      print_endline @@ "[Day " ^ string_of_int day.number ^ "]";
      print_endline @@ "  Part1: " ^ day.part1;
      print_endline @@ "  Part2: " ^ day.part2)
    res;

  print_endline "";
  print_endline "Timings";
  print_endline "";

  List.iter
    (fun day ->
      print_endline @@ "  Day " ^ string_of_int day.number ^ ": "
      ^ format_colored_duration 1.0 day.duration)
    res;

  print_endline "";
  print_endline @@ "Sum: "
  ^ format_colored_duration 25.0 runtime_sum
  ^ " (sum of individual runtimes)";
  print_endline @@ "Total: "
  ^ format_colored_duration 30.0 total_duration
  ^ " (total runtime, IO included)";

  print_endline "";
  print_endline "Done"

let run_days ctx =
  let start_time = Sys.time () in
  let res = List.map (run_day ctx) days in
  let total_duration = Sys.time () -. start_time in
  display_results res total_duration

let new_ctx () = { session = session () }
