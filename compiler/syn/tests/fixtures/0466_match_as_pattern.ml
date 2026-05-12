let x =
  match xs with
  | y :: _ as full -> (y, full)
  | [] -> (0, [])
