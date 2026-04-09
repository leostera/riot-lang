open Std
module Test = Std.Test
module Kernel = Kernel_new

let ( let* ) = Result.and_then

let lift_system_time = function
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Error.to_string
    (Kernel.Error.of_time_system_time error))

let lift_monotonic = function
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Error.to_string (Kernel.Error.of_time_monotonic error))

let test_of_parts_roundtrips = fun _ctx ->
  let* value = lift_system_time (Kernel.Time.SystemTime.of_parts ~secs:42 ~nanos:123_456_789) in
  let (secs, nanos) = Kernel.Time.SystemTime.to_parts value in
  if secs = 42 && nanos = 123_456_789 then
    Ok ()
  else
    Error "expected system time parts roundtrip"

let test_now_is_at_or_after_epoch = fun _ctx ->
  let* now = lift_system_time (Kernel.Time.SystemTime.now ()) in
  if Kernel.Time.SystemTime.compare now Kernel.Time.SystemTime.epoch >= 0 then
    Ok ()
  else
    Error "expected current system time to be at or after the epoch"

let test_now_has_valid_nanoseconds = fun _ctx ->
  let* now = lift_system_time (Kernel.Time.SystemTime.now ()) in
  let nanos = Kernel.Time.SystemTime.subsec_nanos now in
  if nanos >= 0 && nanos < 1_000_000_000 then
    Ok ()
  else
    Error "expected current system time nanoseconds to be normalized"

let test_monotonic_now_is_non_decreasing = fun _ctx ->
  let* earlier = lift_monotonic (Kernel.Time.Monotonic.now ()) in
  let* later = lift_monotonic (Kernel.Time.Monotonic.now ()) in
  if Kernel.Time.Monotonic.compare earlier later <= 0 then
    Ok ()
  else
    Error "expected monotonic now to be non-decreasing"

let test_monotonic_now_has_valid_nanoseconds = fun _ctx ->
  let* now = lift_monotonic (Kernel.Time.Monotonic.now ()) in
  let nanos = Kernel.Time.Monotonic.subsec_nanos now in
  if nanos >= 0 && nanos < 1_000_000_000 then
    Ok ()
  else
    Error "expected monotonic now nanoseconds to be normalized"

let tests = [
  Test.case "Time.SystemTime of_parts roundtrips" test_of_parts_roundtrips;
  Test.case "Time.SystemTime now is at or after epoch" test_now_is_at_or_after_epoch;
  Test.case "Time.SystemTime now has normalized nanoseconds" test_now_has_valid_nanoseconds;
  Test.case "Time.Monotonic now is non-decreasing" test_monotonic_now_is_non_decreasing;
  Test.case "Time.Monotonic now has normalized nanoseconds" test_monotonic_now_has_valid_nanoseconds;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_system_time_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
