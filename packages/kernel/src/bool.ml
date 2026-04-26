open Prelude

type t = bool

let true_ = true

let false_ = false

let equal = Caml_runtime.equal

let compare = Order.compare

let not = fun value -> Caml_runtime.not_bool ~value

let to_string = fun value ->
  if value then
    "true"
  else
    "false"
