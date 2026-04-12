type t = float

let equal = Caml_runtime.equal

let compare = Caml_runtime.compare

let of_int = Caml_runtime.float_of_int

let to_int = Caml_runtime.int_of_float

let to_string = fun ?(precision = 6) value ->
  let precision =
    match Int.compare precision 0 with
    | -1 -> 0
    | _ -> precision
  in
  let format = String.concat "" [ "%."; Int.to_string precision; "f" ] in
  Caml_runtime.format_float format value

let rem = Caml_runtime.rem_float

let sqrt = Caml_runtime.sqrt_float

let floor = Caml_runtime.floor_float

let ceil = Caml_runtime.ceil_float

let pow = Caml_runtime.pow_float

let round = Caml_runtime.round_float
