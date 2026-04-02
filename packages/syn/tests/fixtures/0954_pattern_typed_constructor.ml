let f x =
  match x with
  | (Some x: int option) -> x
  | None -> 0
