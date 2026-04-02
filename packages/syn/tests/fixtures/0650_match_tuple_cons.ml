let x =
  match pair with
  | h :: t, x -> h + x
  | _ -> 0
