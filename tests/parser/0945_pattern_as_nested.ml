let f x =
  match x with
  | (x :: xs as list), y -> (list, y)
  | _ -> ([], 0)
