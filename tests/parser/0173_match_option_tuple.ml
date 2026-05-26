let x =
  match (Some 1, Some 2) with
  | Some a, Some b -> a + b
  | _ -> 0
