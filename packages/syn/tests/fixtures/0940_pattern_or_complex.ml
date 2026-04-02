let f x =
  match x with
  | Ok (1 | 2)
  | Error "small" -> true
  | _ -> false
