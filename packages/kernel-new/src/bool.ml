type t = bool

let true_ = true

let false_ = false

let equal = Primitives.equal

let compare = Primitives.compare

let not = Primitives.not_bool

let to_string = fun value ->
  if value then
    "true"
  else
    "false"
