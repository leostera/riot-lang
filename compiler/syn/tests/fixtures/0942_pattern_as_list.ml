let f x =
  match x with
  | x :: xs as list -> list
  | [] -> []
