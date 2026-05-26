let x =
  match { outer = { inner = 5 } } with
  | { outer={ inner=y } } -> y
