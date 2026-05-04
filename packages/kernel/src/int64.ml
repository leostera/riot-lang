type t = int64

let zero = 0L

let min_int = (-0x8000_0000_0000_0000L)

let max_int = 0x7fff_ffff_ffff_ffffL

let from_int = Caml_runtime.int64_of_int

let to_int = Caml_runtime.int64_to_int

let logand = Caml_runtime.int64_logand

let logor = Caml_runtime.int64_logor

let logxor = Caml_runtime.int64_logxor

let lognot = fun value -> logxor value (-1L)

let shift_left = Caml_runtime.shift_left_int64

let shift_right = Caml_runtime.shift_right_int64

let shift_right_logical = Caml_runtime.shift_right_logical_int64

let abs = fun value ->
  match Order.compare value 0L with
  | Order.LT -> Caml_runtime.int64_neg value
  | Order.EQ
  | Order.GT -> value

let neg = Caml_runtime.int64_neg

let add = Caml_runtime.int64_add

let sub = Caml_runtime.int64_sub

let mul = Caml_runtime.int64_mul

let div = Caml_runtime.int64_div

let rem = Caml_runtime.int64_rem

let succ = fun value -> add value 1L

let pred = fun value -> sub value 1L

let from_float = Caml_runtime.int64_of_float

let to_float = Caml_runtime.int64_to_float

external bits_of_float: float -> int64 = "caml_int64_bits_of_float"

external float_of_bits: int64 -> float = "caml_int64_float_of_bits"

let from_int32 = Caml_runtime.int64_of_int32

let to_int32 = Caml_runtime.int64_to_int32

external format: string -> int64 -> string = "caml_int64_format"

external parse_unchecked: string -> int64 = "caml_int64_of_string"

let from_string = parse_unchecked

let parse = fun value ->
  try Some (parse_unchecked value) with
  | _ -> None

let from_string_opt = parse

let to_string = fun value -> format "%d" value

let hash = to_int

let equal = Caml_runtime.equal

let compare = Order.compare
