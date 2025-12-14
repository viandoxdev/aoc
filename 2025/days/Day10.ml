open Utils

let parse_int_list s = String.split_on_char ',' s |> List.map int_of_string
let parse_lights s =
    String.fold_right (fun c a -> match c with
        | '#' -> 2 * a + 1
        | '.' -> 2 * a
        | _ -> a) s 0
let parse_buttons s =
    String.split_on_char ' ' (trim_end_nl s) 
        |> List.map (fun s -> 
            parse_int_list (String.sub s 1 (String.length s - 2)) 
                |> List.fold_left (fun a d -> a + Int.shift_left 1 d) 0)

let parse input =
    let input = trim_end_nl input in
    String.split_on_char '\n' input 
        |> List.map (fun l -> 
            let a, b = string_split_once " " l in
            let b, c = string_split_once "{" b in
            let c = String.sub c 0 (String.length c - 1) in

            String.length a - 2,
            parse_lights a,
            parse_buttons b |> Array.of_list,
            parse_int_list c |> Array.of_list)

type 'a control = Continue of 'a | Stop of 'a

let select i a =
    let rec aux i o = 
        if i = 0 then [] 
        else if Int.logand i 1 = 1 then a.(o) :: aux (Int.shift_right i 1) (o + 1)
        else aux (Int.shift_right i 1) (o + 1) in
    aux i 0

let turn_on (_, lights, buttons, _) =
    let k = Array.length buttons in
    let m = Int.shift_left 1 k in
    range 0 (m - 1) 
        |> List.filter (fun i -> List.fold_left Int.logxor 0 (select i buttons) = lights)
        |> List.map Ocaml_intrinsics.Int.count_set_bits
        |> List.fold_left min (k + 1)

let fold_bin f acc bin =
    let rec aux off bin acc = 
        if bin = 0 then acc
        else 
            let skip = Ocaml_intrinsics.Int.count_trailing_zeros bin in
            aux (off + skip + 1) (Int.shift_right bin (skip + 1))  (f acc (off + skip))
    in aux 0 bin acc

(* matrices are Arrays of rows *)
let mat_dim m = Array.length m, Array.length m.(0)

(* matrix product of a by b *)
let mat_prod a b =
    let (n, m), (_, p) = mat_dim a, mat_dim b in
    Array.init_matrix n p (fun i j -> range_sum (fun k -> a.(i).(k) * b.(k).(j)) 0 (m - 1))

let mat_vect_prod a x =
    let (n, m) = mat_dim a in
    Array.init n (fun i -> range_sum (fun j -> a.(i).(j) * x.(j)) 0 (m - 1))

let identity n =
    Array.init_matrix n n (fun i j -> int_of_bool (i = j))

(* n by n matrix that swaps rows i1 and i2 *)
let swap_rows n i1 i2 =
    Array.init_matrix n n (fun i j -> int_of_bool ((i = i1 && j = i2) || (i = i2 && j = i1) || (i <> i1 && i <> i2 && i = j)))

(* n by n matrix that scales row k by s *)
let scale_row n k s =
    Array.init_matrix n n (fun i j -> if i = k && j = k then s else int_of_bool (i = j))

(* n by n matrix that adds s times row i1 to row i2 *)
let transvect_row n i1 i2 s =
    Array.init_matrix n n (fun i j -> if i = j then 1 else if i = i2 && j = i1 then s else 0)

let rec gcd a = function
    | 0 -> a
    | b -> gcd b (a mod b)

let lcm a b = a * b / (gcd a b)

let mat_copy mat =
    Array.init (Array.length mat) (fun i -> Array.copy mat.(i))
    
let echelon mat =
    let n, m = mat_dim mat in
    range_fold (fun (p, r) j -> match range_find (fun i -> mat.(i).(j) <> 0) r (n - 1) with
        | Some i0 -> (
            (* fetch first non 0 value in column, swap with first row *)
            let p = if i0 <> r then begin
                let row = mat.(i0) in
                mat.(i0) <- mat.(r);
                mat.(r) <- row;
                mat_prod (swap_rows n r i0) p
            end else p in
            
            let a = mat.(r).(j) in

            range_fold (fun p i -> let b = mat.(i).(j) in if b = 0 then p else begin
                let c = abs (lcm a b) in
                let ka, kb = c / a, c / b in

                for j = 0 to (m - 1) do
                    mat.(i).(j) <- mat.(i).(j) * kb - mat.(r).(j) * ka
                done;

                mat_prod (transvect_row n r i (-ka)) (mat_prod (scale_row n i kb) p)
            end) p (r + 1) (n - 1), r + 1
        )
        | None -> p, r
    ) (identity n, 0) 0 (m - 1) |> fst

(* find the minimum L1 norm of the solutions to m x = y with m in echelon form and 0 <= x <= z *)
let solve_echelon m y z =
    let n, k = mat_dim m in
    let x = Array.make k (-1) in

    (* We are looking for solutions with positive integers, and we
       need to compute partial norms (with unfixed variables) to prune 
       useless branches, hence `max x 0` instead of `abs x` *)
    let norm_of = Array.fold_left (fun a x -> a + (max x 0)) 0 in

    (* get the index of the last free variable in tow r (last since we solve top bottom) *)
    let free_variable_in_row x r =
        let rec aux i = if x.(i) = -1 && m.(r).(i) <> 0 then Some i else if i = 0 then None else aux (i - 1) in
        aux (Array.length x - 1) in

    (* Backtracking branch and bound algorithm to find the minimal solution *)
    let rec aux row best =
        let norm = norm_of x in
        (* If partial norm is already more than the best we've found, no need to go any further *)
        if norm > best then best else
        (* Depending on the matrix, we may reach this point with some variables
           left unfixed, but that means those variables don't change the end result 
           and so we just need to set them to 0 (to minimize norm).

           Moreover since all we care about is the norm, and our norm function 
           already assumes 0 for unfixed variables, we don't even need to handle 
           that case in the first place. *)
        if row < 0 then min best norm
        else match free_variable_in_row x row with
            (* No more variables to fix in this row, go to the next one *)
            | None -> aux (row - 1) best
            | Some free_index -> (
                (* How many variables need to be fixed in this row *)
                let free_in_row = range_sum (fun j -> int_of_bool (x.(j) = -1 && m.(row).(j) <> 0)) 0 (k - 1) in

                if free_in_row > 1 then begin
                    (* More than one variable needs to be fixed in this row.
                       We can't really guess anything here, just try everything *)       
                    let new_best = 
                        (* Try all the values fitting in the range *)
                        range_fold (fun best v -> begin
                            x.(free_index) <- v;
                            aux row best
                        end) best 0 z.(free_index) in
                    (* Backtrack *)
                    x.(free_index) <- (-1);
                    new_best
                end else
                    (* There is only one free variabled in this row, compute its value 
                       (from the others) and check compatibility *)
                    let sum = y.(row) - (range_sum (fun j -> if x.(j) != -1 then m.(row).(j) * x.(j) else 0) 0 (k - 1)) in
                    let coef = m.(row).(free_index) in
                    let value = sum / coef in

                    if value * coef <> sum || value < 0 || value > z.(free_index) then
                        (* System is incompatible with our constraints *)
                        best
                    else begin
                        (* Only one possible value, and it works, go on *)
                        x.(free_index) <- value;
                        let new_best = aux (row - 1) best in
                        (* Backtrack *)
                        x.(free_index) <- -1;
                        new_best
                    end
            ) in
    aux (n - 1) Int.max_int

let solve_part1 machines =
    List.map turn_on machines |> List.fold_left (+) 0

let min_joltage (n, _, buttons, joltages) =
    (* Linear algebra interpretation:
        - We consider the buttons as a single rectangular n by k matrix B of 0s and 1s.
        - The expected joltages make up a vector J of size n.
        - A solution X to the problem is a vector of size k that satisfies
            B X = J
          The number of button presses is the L1 norm of that vector.
        - X and J are made up of positive integers
        - Since no button removes joltages, we can compute a loose upper bound on each button
          presses, making up a vector M of size k, we are then looking for the minimal (for the L1 norm)
          solution to the following system:
            B X = J and 0 <= X <= M
        - To compute the solutions we first write B in echelon form:
            P B = E
          The system becomes
            E X = P B X = P J and 0 <= X <= M *)
    let k = Array.length buttons in
    let b = Array.init_matrix n k (fun i j -> Int.logand 1 (Int.shift_right buttons.(j) i)) in
    let p = echelon b in
    let j = mat_vect_prod p joltages in
    let m = Array.map (fold_bin (fun a j -> min a joltages.(j)) Int.max_int) buttons in
    solve_echelon b j m

let solve_part2 machines = 
    List.fold_left (fun a m -> a + min_joltage m) 0 machines

let day10 input =
    let machines = parse input in
    let part1 = solve_part1 machines in
    let part2 = solve_part2 machines in

    (string_of_int part1, string_of_int part2)
