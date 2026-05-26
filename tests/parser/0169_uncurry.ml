let uncurry = fun f pair ->
  match pair with
  | x, y -> f x y
