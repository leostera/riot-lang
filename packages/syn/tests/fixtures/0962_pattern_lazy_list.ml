let f x =
  match x with
  | (lazy (x :: xs)) -> x
  | (lazy []) -> 0
