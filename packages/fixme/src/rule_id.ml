open Std

type t = string

let from_string = fun value -> value

let to_string = fun value -> value

let equal = String.equal

let compare = String.compare

let separator_index = fun value -> String.index_of value ~char:':'

let has_package_name = fun value -> Option.is_some (separator_index value)

let split = fun ~default_package value ->
  match separator_index value with
  | Some idx ->
      let package_name = String.sub value ~offset:0 ~len:idx in
      let local_id = String.sub value ~offset:(idx + 1) ~len:(String.length value - idx - 1) in
      (package_name, local_id)
  | None -> (default_package, value)

let package_name = fun ~default_package value ->
  let (package_name, _) = split ~default_package value in
  package_name

let local_id = fun value ->
  let (_, local_id) = split ~default_package:"riot" value in
  local_id

let qualify = fun ~package_name value ->
  if has_package_name value then
    value
  else
    package_name ^ ":" ^ value
