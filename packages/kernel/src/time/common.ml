open Prelude

let nanos_per_second = Int64.from_int 1_000_000_000

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
  let secs = Int64.mul (Int64.from_int (left_secs - right_secs)) nanos_per_second in
  Int64.add secs (Int64.from_int (left_nanos - right_nanos))

let split_ns = fun timeout_ns ->
  let secs = Int64.to_int (Int64.div timeout_ns nanos_per_second) in
  let nanos = Int64.to_int (Int64.rem timeout_ns nanos_per_second) in
  (secs, nanos)
