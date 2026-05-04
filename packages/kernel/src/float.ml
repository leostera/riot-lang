type t = float

external float_of_bits: int64 -> float =
  "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed" [@@ unboxed] [@@ noalloc]

let max_float = float_of_bits 0x7fef_ffff_ffff_ffffL

let min_float = float_of_bits 0x0010_0000_0000_0000L

let infinity = Caml_runtime.div_float 1.0 0.0

let nan = Caml_runtime.div_float 0.0 0.0

let equal = Caml_runtime.equal

let compare = Order.compare

let from_int = Caml_runtime.float_of_int

let to_int = Caml_runtime.int_of_float

let parse_unchecked = Caml_runtime.float_of_string

let from_string = parse_unchecked

let parse = fun value ->
  try Some (parse_unchecked value) with
  | _ -> None

let from_string_opt = parse

let add = Caml_runtime.add_float

let sub = Caml_runtime.sub_float

let mul = Caml_runtime.mul_float

let div = Caml_runtime.div_float

let is_finite = fun value -> equal (Caml_runtime.sub_float value value) 0.0

let is_infinite = fun value -> equal (Caml_runtime.div_float 1.0 value) 0.0

let is_nan = fun value ->
  if equal value value then
    false
  else
    true

let to_string = fun ?(precision = 6) value ->
  let precision =
    match Int.compare precision 0 with
    | Order.LT -> 0
    | Order.EQ
    | Order.GT -> precision
  in
  let format = String.concat "" [ "%."; Int.to_string precision; "f" ] in
  Caml_runtime.format_float format value

let rem = Caml_runtime.rem_float

let abs = fun value ->
  match compare value 0.0 with
  | Order.LT -> Caml_runtime.neg_float value
  | Order.EQ
  | Order.GT -> value

let sqrt = Caml_runtime.sqrt_float

let cbrt = fun value ->
  let exponent = Caml_runtime.div_float 1.0 3.0 in
  match compare value 0.0 with
  | Order.LT -> Caml_runtime.neg_float (Caml_runtime.pow_float (abs value) exponent)
  | Order.EQ
  | Order.GT -> Caml_runtime.pow_float value exponent

let floor = Caml_runtime.floor_float

let ceil = Caml_runtime.ceil_float

let pow = Caml_runtime.pow_float

let round = Caml_runtime.round_float
