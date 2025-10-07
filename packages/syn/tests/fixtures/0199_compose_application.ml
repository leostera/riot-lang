let x =
  let f = fun x -> x * 2 in
  let g = fun x -> x + 1 in
  f (g 5)
