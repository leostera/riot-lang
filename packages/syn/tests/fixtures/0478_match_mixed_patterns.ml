let x =
  match (a, b) with
  | Some c, d :: ds -> c + d
  | _ -> 0
