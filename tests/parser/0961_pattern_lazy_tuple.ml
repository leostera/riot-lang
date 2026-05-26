let f x =
  match x with
  | (lazy (a, b)) -> a + b
