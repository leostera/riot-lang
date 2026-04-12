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

let code = to_int

let lowercase_ascii = fun value ->
  let code = to_int value in
  if code >= to_int 'A' && code <= to_int 'Z' then
    unsafe_of_int (code + 32)
  else
    value

let uppercase_ascii = fun value ->
  let code = to_int value in
  if code >= to_int 'a' && code <= to_int 'z' then
    unsafe_of_int (code - 32)
  else
    value

let chr = fun value ->
  match of_int value with
  | Some value -> value
  | None -> raise (Invalid_argument "Char.chr")
