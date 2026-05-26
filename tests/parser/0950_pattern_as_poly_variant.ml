let f x =
  match x with
  | `Some x as v -> v
  | `None as v -> v
