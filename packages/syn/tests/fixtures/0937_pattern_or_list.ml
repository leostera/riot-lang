let f x =
  match x with
  | []
  | [ _ ] -> "short"
  | _ -> "long"
