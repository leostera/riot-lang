type t = int32

let zero = 0l

let min_int = (-0x8000_0000l)

let max_int = 0x7fff_ffffl

external from_int: int -> int32 = "%int32_of_int"

external to_int: int32 -> int = "%int32_to_int"

let neg = Caml_runtime.int32_neg

let abs = fun value ->
  if Caml_runtime.less_than (Caml_runtime.compare value 0l) 0 then
    neg value
  else
    value

let add = Caml_runtime.int32_add

let sub = Caml_runtime.int32_sub

let mul = Caml_runtime.int32_mul

let div = Caml_runtime.int32_div

let rem = Caml_runtime.int32_rem

let logand = Caml_runtime.int32_logand

let logor = Caml_runtime.int32_logor

let logxor = Caml_runtime.int32_logxor

let shift_left = Caml_runtime.shift_left_int32

let shift_right = Caml_runtime.shift_right_int32

let shift_right_logical = Caml_runtime.shift_right_logical_int32

let from_float = Caml_runtime.int32_of_float

let to_float = Caml_runtime.int32_to_float

external format: string -> int32 -> string = "caml_int32_format"

external parse_unchecked: string -> int32 = "caml_int32_of_string"

let parse = fun value ->
  try Some (parse_unchecked value) with
  | _ -> None

let to_string = fun value -> format "%d" value

let equal = Caml_runtime.equal

let compare = Caml_runtime.compare
