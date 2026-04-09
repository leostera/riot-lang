type 'value t = 'value option =
  | None
  | Some of 'value

let map = fun fn ->
  function
  | Some value -> Some (fn value)
  | None -> None

let is_some = fun value ->
  match value with
  | Some _ -> true
  | None -> false

let is_none = fun value ->
  match value with
  | Some _ -> false
  | None -> true

let unwrap_or = fun value ~default ->
  match value with
  | Some value -> value
  | None -> default
