let f x =
  match x with
  | (0, _)
  | (_, 0) -> "has zero"
  | _ -> "no zero"
