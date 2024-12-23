open Utils

module Computer = struct
  type t = char * char

  let zero = int_of_char 'a'
  let index (a, b) = ((int_of_char a - zero) * 26) + (int_of_char b - zero)

  let of_index i =
    (char_of_int @@ (zero + (i / 26)), char_of_int @@ (zero + (i mod 26)))

  let all_computers = Iter.(0 -- ((26 * 26) - 1) |> map of_index)

  let trio_key a b c =
    let a, b, c = (index a, index b, index c) in
    let mi = min (min a b) c in
    let ma = max (max a b) c in
    let se =
      if b > mi && b < ma then b else if c > mi && c < ma then c else a
    in
    (mi * 26 * 26 * 26 * 26) + (se * 26 * 26) + ma

  let compare a b = Int.compare (index a) (index b)
end

module ComputerSet = Set.Make (Computer)

module Map = struct
  type 'a t = 'a array

  let make d = Array.make (26 * 26) d

  let add m k v =
    let index = Computer.index k in
    m.(index) <- v

  let get m k = m.(Computer.index k)
end

module P = struct
  open Angstrom

  let computer = both any_char any_char
  let connection = both (computer <* char '-') (computer <* char '\n')

  let parse =
    parse_string ~consume:All (many connection)
    %> Result.get_ok
    %> List.fold_left
         (fun m (a, b) ->
           Map.add m a (ComputerSet.add b (Map.get m a));
           Map.add m b (ComputerSet.add a (Map.get m b));
           m)
         (Map.make ComputerSet.empty)
end

let part1 m =
  let open Map in
  let open Iter in
  let open Computer in
  0 -- 25
  |> map (fun c -> of_index @@ (((int_of_char 't' - zero) * 26) + c))
  |> flat_map (fun c ->
         get m c
         |> of_set (module ComputerSet)
         |> diagonal
         |> filter (fun (a, b) -> ComputerSet.mem b (get m a))
         |> map (fun (a, b) -> (c, a, b)))
  |> map (fun (a, b, c) -> (trio_key a b c, (a, b, c)))
  |> sort_uniq ~cmp:(fun (a, _) (b, _) -> Int.compare a b)
  |> length

let part2 m =
  let open Map in
  let open Iter in
  let open Computer in
  let max_size = ref 0 in
  let max_clique = ref ComputerSet.empty in

  let rec aux r p x =
    let open ComputerSet in
    (if is_empty p && is_empty x then
       let card = cardinal r in
       if card > !max_size then (
         max_size := card;
         max_clique := r));
    ignore
    @@ fold
         (fun v (p, x) ->
           aux (add v r) (inter p (get m v)) (inter x (get m v));
           (remove v p, add v x))
         p (p, x)
  in

  aux ComputerSet.empty
    (all_computers
    |> filter (fun c -> not @@ ComputerSet.is_empty (get m c))
    |> to_set (module ComputerSet))
    ComputerSet.empty;

  of_set (module ComputerSet) !max_clique
  |> sort ~cmp:Computer.compare
  |> map (fun (a, b) -> String.init 2 (fun i -> if i = 0 then a else b))
  |> intersperse "," |> concat_str

let day23 input =
  let map = P.parse input in
  (string_of_int @@ part1 map, part2 map)
