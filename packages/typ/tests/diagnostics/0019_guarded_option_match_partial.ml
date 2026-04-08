let classify input =
  match input with
  | Some value when true -> value
  | None -> 0
