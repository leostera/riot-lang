let x =
  match v with
  | `Point (x, y) -> x + y
  | _ -> 0
