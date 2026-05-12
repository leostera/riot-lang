let f x =
  match x with
  | '0' .. '9' -> "digit"
  | 'a' .. 'f' -> "hex"
  | _ -> "other"
