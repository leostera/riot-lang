(* With labeled params - simplified *)

let f (type a) ~label (x: a) = label

(* With optional params *)

let g (type a) ?opt (x: a) = x

(* Nested in let rec *)

let rec fold (type a acc) (iter: a t) ~init ~fn : acc =
  match next iter with
  | None -> init
  | Some (x, iter') -> fold iter' ~init ~fn

(* Combined with first-class modules *)

let process (type item state) (handler: item) (x: item) = x
