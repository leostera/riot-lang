let x =
  try
    match f y with
    | Some z -> z
    | None -> raise E
  with
  | E -> 0
