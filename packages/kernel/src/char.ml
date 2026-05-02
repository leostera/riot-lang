open Prelude

type t = char

let equal = Caml_runtime.equal

let compare = Order.compare

let from_int_unchecked = Caml_runtime.char_of_int

let from_int = fun value ->
  if value < 0 || value > 255 then
    None
  else
    Some (from_int_unchecked value)

let to_int = Caml_runtime.int_of_char

let code = to_int

let lowercase_ascii = fun value ->
  let code = to_int value in
  if code >= to_int 'A' && code <= to_int 'Z' then
    from_int_unchecked (code + 32)
  else
    value

let uppercase_ascii = fun value ->
  let code = to_int value in
  if code >= to_int 'a' && code <= to_int 'z' then
    from_int_unchecked (code - 32)
  else
    value
