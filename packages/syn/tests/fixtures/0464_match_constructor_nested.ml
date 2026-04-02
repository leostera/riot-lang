let x =
  match Some (Some (Some 1)) with
  | Some (Some (Some y)) -> y
  | _ -> 0
