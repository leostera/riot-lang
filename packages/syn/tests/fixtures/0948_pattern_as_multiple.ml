let f x =
  match x with
  | ((a as x), b) as y -> (x, y)
