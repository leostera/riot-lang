let x =
  match result with
  | Ok (Ok (Ok y)) -> y
  | _ -> 0
