(* Functional priority queue via leftist heap. *)
type heap =
  | E
  | T of int * int * heap * heap

let rank = function
  | E -> 0
  | T (r, _, _, _) -> r

let make x a b =
  if rank a >= rank b then T (rank b + 1, x, a, b)
  else T (rank a + 1, x, b, a)

let rec merge h1 h2 =
  match h1, h2 with
  | E, h | h, E -> h
  | T (_, x, a1, b1), T (_, y, a2, b2) ->
      if x <= y then make x a1 (merge b1 h2)
      else make y a2 (merge h1 b2)

let insert x h = merge (T (1, x, E, E)) h

let rec pop = function
  | E -> None
  | T (_, x, a, b) -> Some (x, merge a b)

let () =
  let h =
    List.fold_left (fun acc x -> insert x acc) E [ 7; 3; 9; 1; 4 ]
  in
  let rec drain = function
    | E -> ()
    | h ->
        begin match pop h with
        | None -> ()
        | Some (x, h') ->
            Printf.printf "%d " x;
            drain h'
        end
  in
  drain h;
  print_newline ()
