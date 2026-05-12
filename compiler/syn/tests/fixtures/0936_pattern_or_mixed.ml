let f x =
  match x with
  | 0
  | 1
  | Some 2 -> true
  | _ -> false
