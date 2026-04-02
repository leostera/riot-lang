let f x =
  match x with
  | Some x as opt -> opt
  | None -> None
