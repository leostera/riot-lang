let f x =
  match x with
  | exception Failure msg -> Error msg
  | y -> Ok y
