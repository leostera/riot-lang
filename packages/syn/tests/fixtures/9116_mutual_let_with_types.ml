(* Mutually recursive let bindings with type annotations *)

let rec map : 'a list -> ('a -> 'b) -> 'b list = fun xs f ->
  match xs with
  | [] -> []
  | x :: xs -> f x :: map xs f

and fold_left : 'a -> ('a -> 'b -> 'a) -> 'b list -> 'a = fun acc f xs ->
  match xs with
  | [] -> acc
  | x :: xs -> fold_left (f acc x) f xs
