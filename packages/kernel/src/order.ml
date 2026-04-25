type t =
  | LT
  | EQ
  | GT

let compare = fun left right ->
  let order = Caml_runtime.compare left right in
  if Caml_runtime.less_than order 0 then
    LT
  else
    if Caml_runtime.greater_than order 0 then
      GT
    else EQ
