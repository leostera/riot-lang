open Prelude

type t = char

let equal = Caml_runtime.equal

let compare = Caml_runtime.compare

let unsafe_of_int = Caml_runtime.char_of_int

let of_int = fun value ->
  if value < 0 || value > 255 then
    None
  else
    Some (unsafe_of_int value)

let to_int = Caml_runtime.int_of_char
