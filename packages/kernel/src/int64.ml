type t = int64

let of_int = Caml_runtime.int64_of_int

let to_int = Caml_runtime.int64_to_int

let neg = Caml_runtime.int64_neg

let add = Caml_runtime.int64_add

let sub = Caml_runtime.int64_sub

let mul = Caml_runtime.int64_mul

let div = Caml_runtime.int64_div

let rem = Caml_runtime.int64_rem

let succ = fun value -> add value 1L

let pred = fun value -> sub value 1L

let of_float = Caml_runtime.int64_of_float

let to_float = Caml_runtime.int64_to_float

let of_int32 = Caml_runtime.int64_of_int32

let hash = to_int

let equal = Caml_runtime.equal

let compare = Caml_runtime.compare
