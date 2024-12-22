open Utils

type key = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | A | U | L | D | R
type keypad = { coords : (int * int) option array; gap : int * int }

let key_of_char = function
  | '0' -> N0
  | '1' -> N1
  | '2' -> N2
  | '3' -> N3
  | '4' -> N4
  | '5' -> N5
  | '6' -> N6
  | '7' -> N7
  | '8' -> N8
  | '9' -> N9
  | 'A' -> A
  | _ -> failwith "No such key"

let int_of_key = function
  | N0 -> 0
  | N1 -> 1
  | N2 -> 2
  | N3 -> 3
  | N4 -> 4
  | N5 -> 5
  | N6 -> 6
  | N7 -> 7
  | N8 -> 8
  | N9 -> 9
  | A -> 10
  | U -> 11
  | L -> 12
  | D -> 13
  | R -> 14

let dist (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)

let parse input =
  String.split_on_char '\n' input
  |> List.filter (( <> ) "")
  |> List.map
       Iter.(
         fun s ->
           ( of_str s |> map key_of_char |> to_list,
             int_of_string (of_str s |> filter is_digit |> to_str) ))

let keypad_coords l =
  let a = Array.make 15 None in
  List.iter (fun (k, x, y) -> a.(int_of_key k) <- Some (x, y)) l;
  a

let make_memo = Hashtbl.create 2048

let rec path ~memo f t =
  let open Iter in
  function
  | [] -> 1
  | pad :: pads -> (
      let k = List.length pads in
      match Hashtbl.find_opt memo (k, f, t) with
      | Some r -> r
      | None ->
          let gx, gy = pad.gap in
          let sx, sy = Option.get @@ pad.coords.(int_of_key f) in
          let ex, ey = Option.get @@ pad.coords.(int_of_key t) in
          let dx, dy = (ex - sx, ey - sy) in
          let hor = repeat (if dx > 0 then R else L) |> take (abs dx) in
          let ver = repeat (if dy > 0 then D else U) |> take (abs dy) in
          let presses =
            if dx = 0 then singleton (snoc ver A)
            else if dy = 0 then singleton (snoc hor A)
            else if sy = gy && ex = gx then singleton (snoc (append ver hor) A)
            else if sx = gx && ey = gy then singleton (snoc (append hor ver) A)
            else doubleton (snoc (append ver hor) A) (snoc (append hor ver) A)
          in

          let res = presses |> map (path_through ~memo pads) |> min_exn in

          Hashtbl.add memo (k, f, t) res;

          res)

and path_through ~memo p i =
  let open Iter in
  i |> fold_map (fun prev cur -> (cur, path ~memo prev cur p)) A |> sum

let numeric_pad =
  {
    coords =
      keypad_coords
        [
          (N7, 0, 0);
          (N8, 1, 0);
          (N9, 2, 0);
          (N4, 0, 1);
          (N5, 1, 1);
          (N6, 2, 1);
          (N1, 0, 2);
          (N2, 1, 2);
          (N3, 2, 2);
          (N0, 1, 3);
          (A, 2, 3);
        ];
    gap = (0, 3);
  }

let directional_pad =
  {
    coords =
      keypad_coords [ (U, 1, 0); (A, 2, 0); (L, 0, 1); (D, 1, 1); (R, 2, 1) ];
    gap = (0, 0);
  }

let solve pads codes =
  let open Iter in
  let memo = make_memo in
  of_list codes
  |> map (fun (keys, n) -> n * path_through ~memo pads (of_list keys))
  |> sum

let part1 = [ numeric_pad; directional_pad; directional_pad ]
let part2 = numeric_pad :: (Array.to_list @@ Array.make 25 directional_pad)

let day21 input =
  let codes = parse input in
  (string_of_int @@ solve part1 codes, string_of_int @@ solve part2 codes)
