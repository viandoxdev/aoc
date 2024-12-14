open Utils

type robot = { p : int * int; v : int * int }

module P = struct
  open Angstrom

  let point = both (signed_digits <* char ',') signed_digits

  let robot =
    both (string "p=" *> point <* whitespace) (string "v=" *> point <* char '\n')
    >>| fun (p, v) -> { p; v }

  let parse = Result.get_ok % parse_string ~consume:All (many robot)
end

let width = 101
let height = 103

let ( %. ) a b =
  let m = a mod b in
  if m < 0 then b + m else m

let step n { p = px, py; v = vx, vy } =
  { p = ((px + (n * vx)) %. width, (py + (n * vy)) %. height); v = (vx, vy) }

let part1 robots =
  let open Iter in
  let mx, my = (width / 2, height / 2) in
  let tl, tr, bl, br =
    of_list robots
    |> map (fun r -> step 100 r)
    |> fold
         (fun (tl, tr, bl, br) { p = px, py; _ } ->
           match () with
           | _ when px < mx && py < my -> (tl + 1, tr, bl, br)
           | _ when px > mx && py < my -> (tl, tr + 1, bl, br)
           | _ when px < mx && py > my -> (tl, tr, bl + 1, br)
           | _ when px > mx && py > my -> (tl, tr, bl, br + 1)
           | _ -> (tl, tr, bl, br))
         (0, 0, 0, 0)
  in
  tl * tr * bl * br

let part2 robots =
  let grid = Array.make_matrix width height false in
  let open Iter in
  let rec solve n =
    if n > width * height then failwith "Couln't find the tree :("
    else (
      List.iter
        (fun r ->
          let x, y = (step (n - 1) r).p in
          grid.(x).(y) <- false)
        robots;

      List.iter
        (fun r ->
          let x, y = (step n r).p in
          grid.(x).(y) <- true)
        robots;

      let count_consecutives arr =
        let max, _ =
          snoc (of_array arr) false
          |> fold
               (fun (max, cur) -> function
                 | true -> (max, cur + 1)
                 | false when cur > max -> (cur, 0)
                 | false -> (max, 0))
               (0, 0)
        in
        max
      in

      let xs =
        0 -- (width - 1) |> map (count_consecutives % Array.get grid) |> sum
      in

      if xs >= 200 then n else solve (n + 1))
  in

  solve 0

let day14 input =
  let robots = P.parse input in
  (string_of_int @@ part1 robots, string_of_int @@ part2 robots)
