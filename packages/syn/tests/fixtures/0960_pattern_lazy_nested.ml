let f x =
  match x with
  | (lazy (Some y)) -> y
  | (lazy None) -> 0
