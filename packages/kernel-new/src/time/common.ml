open Prelude

let nanos_per_second = 1_000_000_000L

let int64_of_int = Caml_runtime.int64_of_int

let int64_to_int = Caml_runtime.int64_to_int

let int64_add = Caml_runtime.int64_add

let int64_mul = Caml_runtime.int64_mul

let int64_div = Caml_runtime.int64_div

let int64_rem = Caml_runtime.int64_rem

let validate_nanos = fun nanos ->
  if nanos < 0 || nanos >= 1_000_000_000 then
    Result.Error ()
  else
    Result.Ok ()

let compare_parts = fun ~left_secs ~left_nanos ~right_secs ~right_nanos ->
  let secs_order = Int.compare left_secs right_secs in
  if secs_order = 0 then
    Int.compare left_nanos right_nanos
  else
    secs_order

let diff_ns = fun ~left_secs ~left_nanos ~right_secs ~right_nanos ->
  let secs = int64_mul (int64_of_int (left_secs - right_secs)) nanos_per_second in
  int64_add secs (int64_of_int (left_nanos - right_nanos))

let split_ns = fun timeout_ns ->
  let secs = int64_to_int (int64_div timeout_ns nanos_per_second) in
  let nanos = int64_to_int (int64_rem timeout_ns nanos_per_second) in
  (secs, nanos)
