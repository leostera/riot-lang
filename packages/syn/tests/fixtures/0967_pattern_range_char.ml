let f x =
  match x with
  | 'a' .. 'z' -> "lowercase"
  | 'A' .. 'Z' -> "uppercase"
  | _ -> "other"
