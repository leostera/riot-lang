type 'a option =
  | None
  | Some of 'a

let classify input =
  match input with
  | Some value when true -> value
  | None -> 0
