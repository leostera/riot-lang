let f x =
  match x with
  | Result.Ok (Option.Some y) -> y
  | _ -> 0
