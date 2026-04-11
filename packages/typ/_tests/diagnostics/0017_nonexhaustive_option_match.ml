type 'a option =
  | None
  | Some of 'a

let unwrap input =
  match input with
  | Some value -> value
