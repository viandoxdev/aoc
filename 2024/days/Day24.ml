open Utils

type gate = Or | And | Xor
type wire = X of int | Y of int | Z of int | Other of int

let string_of_wire = function
  | X i -> "x" ^ Printf.sprintf "%02i" i
  | Y i -> "y" ^ Printf.sprintf "%02i" i
  | Z i -> "z" ^ Printf.sprintf "%02i" i
  | Other i ->
      let a, b, c =
        map_triple
          (fun x -> char_of_int @@ (int_of_char 'a' + (i / x mod 26)))
          (26 * 26, 26, 1)
      in
      String.make 1 a ^ String.make 1 b ^ String.make 1 c

let string_of_gate = function And -> "AND" | Or -> "OR" | Xor -> "XOR"

let gate_eval x y = function
  | Or -> x || y
  | And -> x && y
  | Xor -> if x then not y else y

let wire_of_string s =
  match String.get s 0 with
  | 'x' -> X (int_of_string @@ String.sub s 1 2)
  | 'y' -> Y (int_of_string @@ String.sub s 1 2)
  | 'z' -> Z (int_of_string @@ String.sub s 1 2)
  | _ ->
      let a, b, c =
        map_triple
          (String.get s %> int_of_char %> ( + ) (-int_of_char 'a'))
          (0, 1, 2)
      in
      Other ((a * 26 * 26) + (b * 26) + c)

module P = struct
  open Angstrom

  let wire = take 3 >>| wire_of_string
  let value = char '0' >>| (fun _ -> false) <|> (char '1' >>| fun _ -> true)
  let init = both (wire <* string ": ") (value <* char '\n')

  let gate =
    string "OR"
    >>| (fun _ -> Or)
    <|> (string "AND" >>| fun _ -> And)
    <|> (string "XOR" >>| fun _ -> Xor)

  let con =
    both
      (both (wire <* char ' ') (gate <* char ' '))
      (both (wire <* string " -> ") (wire <* char '\n'))
    >>| fun ((a, b), (c, d)) -> (b, a, c, d)

  let parse input =
    let inits, cons =
      Result.get_ok
      @@ parse_string ~consume:All
           (both (many init <* char '\n') (many con))
           input
    in
    let indices = Hashtbl.create 512 in
    let index_of w =
      match Hashtbl.find_opt indices w with
      | Some i -> i
      | None ->
          let i = Hashtbl.length indices in
          Hashtbl.add indices w i;
          i
    in

    List.iter (fun (_, x, y, z) -> ignore @@ map_triple index_of (x, y, z)) cons;

    let n = Hashtbl.length indices in
    let values = Array.make n None in
    let cons_map = Array.make n [] in
    let indices_rev = Array.make n (Other 0) in
    let cons_rev = Array.make n None in

    List.iter (fun (w, v) -> values.(index_of w) <- Some v) inits;
    List.iter
      (fun (g, x, y, z) ->
        let x, y, z = map_triple index_of (x, y, z) in
        cons_map.(x) <- (g, x, y, z) :: cons_map.(x);
        cons_map.(y) <- (g, y, x, z) :: cons_map.(y);
        cons_rev.(z) <- Some (g, x, y))
      cons;
    Hashtbl.iter (fun w i -> indices_rev.(i) <- w) indices;

    (* DOT Visualization of the input
       let oc = open_out "test.dot" in

       output_string oc "digraph G {\n\n";

       let c = ref 0 in

       List.iter
         (fun (g, x, y, z) ->
           let gs = "g" ^ string_of_int !c in
           c := !c + 1;

           output_string oc @@ "  " ^ gs ^ " [label=\"" ^ string_of_gate g
           ^ "\",shape=box]\n";
           output_string oc @@ "  " ^ string_of_wire x ^ " -> " ^ gs ^ " -> "
           ^ string_of_wire z ^ "\n";
           output_string oc @@ "  " ^ string_of_wire y ^ " -> " ^ gs ^ "\n\n")
         cons;

       let rank c =
         let open Iter in
         output_string oc @@ "  {rank = same; "
         ^ (init id
           |> take_while (fun v -> Hashtbl.mem indices (c v))
           |> map (fun v -> string_of_wire (c v))
           |> intersperse " -> " |> concat_str)
         ^ " [style=invis]; }\n"
       in

       rank (fun x -> X x);
       rank (fun x -> Y x);
       rank (fun x -> Z x);

       output_string oc "}\n";

       close_out oc;
    *)
    (indices, indices_rev, cons_map, cons_rev, values, n)
end

let part1 indices cons_map values n =
  let open Iter in
  let v = Array.copy values in
  let u = ref (Array.make n false) in
  let ub = ref (Array.make n false) in
  let swap () =
    let x = !u in
    u := !ub;
    ub := x
  in

  of_array v |> zip_i
  |> filter (fun (_, v) -> Option.is_some v)
  |> iter (fun (i, _) -> !u.(i) <- true);

  while of_array !u |> exists id do
    of_array !u |> zip_i
    |> filter (fun (_, b) -> b)
    |> iter (fun (i, _) ->
           of_list cons_map.(i)
           |> filter (fun (_, _, y, z) ->
                  Option.is_some v.(y) && Option.is_none v.(z))
           |> iter (fun (g, x, y, z) ->
                  let nv = gate_eval (Option.get v.(x)) (Option.get v.(y)) g in
                  v.(z) <- Some nv;
                  !ub.(z) <- true));
    Array.fill !u 0 n false;
    swap ()
  done;

  init id
  |> take_while (fun x -> Hashtbl.mem indices (Z x))
  |> map (fun x -> Hashtbl.find indices (Z x))
  |> rev
  |> fold
       (fun a x ->
         Int.(logor (shift_left a 1) (if v.(x) = Some true then 1 else 0)))
       0

type selector = Wire of wire | Output of string | Any

type req =
  | Gate of gate * selector * selector * string
  | Equal of selector * wire

type req_err =
  | EqualErr of wire * wire
  | GateErr of gate * wire * wire
  | GateErrPartial of gate * wire

let matches indices index_map cons_map extract reqs =
  let open Iter in
  let state = Hashtbl.create 8 in

  let sel = function
    | Wire w -> Some w
    | Output s -> Hashtbl.find_opt state s
    | Any -> None
  in
  let index_of = Hashtbl.find indices in

  let errors =
    on_list
      (flat_map (function
        | Gate (gate, a, b, id) -> (
            let a, b = (sel a, sel b) in
            match (a, b) with
            | None, None -> empty
            | Some _, None | None, Some _ -> (
                let w =
                  if Option.is_none a then Option.get b else Option.get a
                in
                match
                  of_list cons_map.(index_of w)
                  |> find_pred (fun (g, _, _, _) -> g = gate)
                with
                | Some (_, _, _, z) ->
                    Hashtbl.replace state id index_map.(z);
                    empty
                | None -> singleton (GateErrPartial (gate, w)))
            | Some a, Some b -> (
                match
                  of_list cons_map.(index_of a)
                  |> find_pred (fun (g, _, y, _) ->
                         g = gate && index_map.(y) = b)
                with
                | Some (_, _, _, z) ->
                    Hashtbl.replace state id index_map.(z);
                    empty
                | None -> singleton (GateErr (gate, a, b))))
        | Equal (a, b) ->
            Option.fold ~none:empty
              ~some:(fun a ->
                if a = b then empty else singleton (EqualErr (a, b)))
              (sel a)))
      reqs
  in

  (Hashtbl.find_opt state extract, of_list errors)

let part2 indices index_map cons_map cons_rev =
  let open Iter in
  let matches = matches indices index_map cons_map in

  let errors () =
    let carry, errors =
      matches "carry"
        [
          Gate (And, Wire (X 0), Wire (Y 0), "carry");
          Gate (Xor, Wire (X 0), Wire (Y 0), "sum");
          Equal (Output "sum", Z 0);
        ]
    in
    1 -- 45
    |> fold_map
         (fun carry x ->
           if x = 45 then
             ( None,
               if carry = Some (Z 45) then empty
               else
                 match carry with
                 | Some c -> singleton (EqualErr (c, Z 45))
                 | None -> empty )
           else
             let carry = match carry with Some c -> Wire c | None -> Any in
             matches "next"
               [
                 Gate (And, Wire (X x), Wire (Y x), "carry");
                 Gate (Xor, Wire (X x), Wire (Y x), "sum");
                 Gate (Xor, Output "sum", carry, "out");
                 Gate (And, Output "sum", carry, "a");
                 Equal (Output "out", Z x);
                 Gate (Or, Output "a", Output "carry", "next");
               ])
         carry
    |> cons errors |> flatten
  in

  let index_of = Hashtbl.find indices in

  let swap a b =
    let ia, ib = map_double index_of (a, b) in
    let ag, ax, ay = Option.get cons_rev.(ia) in
    let bg, bx, by = Option.get cons_rev.(ib) in

    cons_rev.(ia) <- Some (bg, bx, by);
    cons_rev.(ib) <- Some (ag, ax, ay);

    cons_map.(bx) <-
      List.map
        (fun (g, x, y, z) ->
          if g = bg && y = by && z == ib then (g, x, y, ia) else (g, x, y, z))
        cons_map.(bx);
    cons_map.(by) <-
      List.map
        (fun (g, x, y, z) ->
          if g = bg && y = bx && z == ib then (g, x, y, ia) else (g, x, y, z))
        cons_map.(by);
    cons_map.(ax) <-
      List.map
        (fun (g, x, y, z) ->
          if g = ag && y = ay && z == ia then (g, x, y, ib) else (g, x, y, z))
        cons_map.(ax);
    cons_map.(ay) <-
      List.map
        (fun (g, x, y, z) ->
          if g = ag && y = ax && z == ia then (g, x, y, ib) else (g, x, y, z))
        cons_map.(ay)
  in

  let pairs = Array.make 4 (Other 0, Other 0) in
  for pair = 0 to 3 do
    match head_exn (errors ()) with
    | EqualErr (a, b) ->
        swap a b;
        pairs.(pair) <- (a, b)
    | GateErr (gate, a, b) ->
        let ia, ib = map_double index_of (a, b) in
        let x, y =
          append
            (of_list cons_map.(ia)
            |> filter (fun (g, _, _, _) -> g = gate)
            |> map (fun (_, _, y, _) -> (b, index_map.(y))))
            (of_list cons_map.(ib)
            |> filter (fun (g, _, _, _) -> g = gate)
            |> map (fun (_, _, y, _) -> (a, index_map.(y))))
          |> head_exn
        in
        swap x y;
        pairs.(pair) <- (x, y)
    | GateErrPartial _ ->
        failwith
          "Can't recover from this error (can't be tested as my input doesn't \
           have this issue)"
  done;

  of_array pairs
  |> flat_map (fun (a, b) -> doubleton a b)
  |> map string_of_wire |> sort |> intersperse "," |> concat_str

let day24 input =
  let indices, index_map, cons_map, cons_rev, values, n = P.parse input in
  ( string_of_int @@ part1 indices cons_map values n,
    part2 indices index_map cons_map cons_rev )
