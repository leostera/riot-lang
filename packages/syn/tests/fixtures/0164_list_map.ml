let rec map = fun f lst ->
  match lst with
  | [] -> []
  | x :: xs -> f x :: map f xs
