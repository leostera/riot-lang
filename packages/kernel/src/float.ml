type t = float

let max_float = 1.797_693_134_862_315_71e+308

let min_float = 2.225_073_858_507_201_38e-308

let equal = Caml_runtime.equal

let compare = Caml_runtime.compare

let of_int = Caml_runtime.float_of_int

let to_int = Caml_runtime.int_of_float

let parse_unchecked = Caml_runtime.float_of_string

let parse = fun value ->
  try Some (parse_unchecked value) with
  | _ -> None

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
    | -1 -> 0
    | _ -> precision
  in
  let format = String.concat "" [ "%."; Int.to_string precision; "f" ] in
  Caml_runtime.format_float format value

let rem = Caml_runtime.rem_float

let abs = fun value ->
  if Caml_runtime.less_than (compare value 0.0) 0 then
    Caml_runtime.neg_float value
  else
    value

let sqrt = Caml_runtime.sqrt_float

external cbrt: float -> float = "caml_cbrt_float" "caml_cbrt"

let floor = Caml_runtime.floor_float

let ceil = Caml_runtime.ceil_float

let pow = Caml_runtime.pow_float

let round = Caml_runtime.round_float
