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
  let open Iter in

  let tmod m key =
    (0 -- (m - 1)) |> map (fun n -> n, of_list robots |> map (step n) |> map (float_of_int % key) |> variance) |> min_exn ~lt: (fun (_, a) (_, b) -> a < b) |> fst
  in
  let (xm, ym) = (tmod width (fun {p=(x,_);_} -> x), tmod height (fun {p=(_,y);_} -> y)) in
  let (_, u, v) = Z.(gcdext (~$ width) (~$ height)) in

  (width * ym * (Z.to_int u) + height * xm * (Z.to_int v)) %. (width * height)

let day14 input =
  let robots = P.parse input in
  (string_of_int @@ part1 robots, string_of_int @@ part2 robots)
