(**
   Time and duration modules.

   Time measurement and duration utilities for timing operations, benchmarking,
   and working with timestamps.

   ## Modules

   - [Duration] - Spans of time with nanosecond precision
   - [Instant] - Monotonic clock for measuring elapsed time
   - [SystemTime] - Wall-clock time for timestamps

   ## Quick Start

   ```ocaml open Std.Time

   (* Create durations *) let timeout = Duration.from_secs 30 in let delay =
   Duration.from_millis 100 in

   (* Measure elapsed time (monotonic) *) let start = Instant.now () in
   expensive_operation (); let elapsed = Instant.elapsed start in Log.info
   "Took %f seconds" (Duration.to_secs_float elapsed)

   (* Get current wall-clock time *) let now = SystemTime.now () in let
   timestamp = SystemTime.to_timestamp now ```

   ## Choosing the Right Type

   | Use Case | Type | |----------|------| | Measure durations | [Instant] | |
   Timeouts/delays | [Duration] | | Timestamps/logging | [SystemTime] | |
   Benchmarking | [Instant] | | Calendar operations | [DateTime] (separate module) |
*)
module Duration = Duration

(** Spans of time with nanosecond precision. See [Duration]. *)
module Instant = Instant

(**
   Monotonic clock measurements immune to system clock changes. See [Instant].
*)
module SystemTime = System_time

(** Wall-clock time for timestamps and calendar operations. See [SystemTime]. *)
type tm = {
  tm_sec: int;
  tm_min: int;
  tm_hour: int;
  tm_mday: int;
  tm_mon: int;
  tm_year: int;
  tm_wday: int;
  tm_yday: int;
  tm_isdst: bool;
}

(** Breaks a Unix timestamp into local calendar fields. *)
val localtime: float -> tm

(** Breaks a Unix timestamp into UTC calendar fields. *)
val gmtime: float -> tm

(**
   Converts local calendar fields back into a Unix timestamp.

   The returned pair includes normalized calendar fields produced by the
   platform.
*)
val mktime: tm -> float * tm
