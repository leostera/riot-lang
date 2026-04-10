type 'a option =
  | None
  | Some of 'a

let classify input =
  match input with
  | _ -> 0
  | Some _ -> 1
