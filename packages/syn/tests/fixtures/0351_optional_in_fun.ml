let x = fun ?y ->
  match y with
  | Some v -> v
  | None -> 0
