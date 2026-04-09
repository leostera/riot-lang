open Prelude

let nanos_per_second = 1_000_000_000L

external int64_of_int: int -> int64 = "%int64_of_int"

external int64_add: int64 -> int64 -> int64 = "%int64_add"

external int64_mul: int64 -> int64 -> int64 = "%int64_mul"

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
