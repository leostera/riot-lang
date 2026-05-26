let x =
  match y with
  | (A _ | B _) as v -> v
  | C _ -> C 0
