type 'a t = 'a option =
  | None
  | Some of 'a

let map = fun fn ->
  function
  | Some value -> Some (fn value)
  | None -> None

let is_some = function
  | Some _ -> true
  | None -> false

let is_none = function
  | Some _ -> false
  | None -> true

let unwrap_or = fun value ~default ->
  match value with
  | Some value -> value
  | None -> default
