(** this is an internal type *)
type timespec = { secs: int; nanos: int }

(** 
A Duration type to represent a span of time, typically used for system timeouts.

Each Duration is composed of a whole number of seconds and a fractional part represented in nanoseconds. If the underlying system does not support nanosecond-level precision, APIs binding a system timeout will typically round up the number of nanoseconds.

Durations implement many common traits, including Add, Sub, and other ops traits. It implements Default by returning a zero-length Duration.
*)
module Duration = struct
  type t = timespec

  let is_zero t = t.secs = 0 && t.nanos = 0
  
  let make ~secs ~nanos = { secs; nanos }
  
  let abs_diff a b = 
    let secs_diff = abs (a.secs - b.secs) in
    let nanos_diff = abs (a.nanos - b.nanos) in
    { secs = secs_diff; nanos = nanos_diff }
  
  let from_days days = { secs = days * 86400; nanos = 0 }
  let from_hours hours = { secs = hours * 3600; nanos = 0 }
  let from_mins mins = { secs = mins * 60; nanos = 0 }
  let from_secs secs = { secs; nanos = 0 }
  let from_millis millis = 
    let secs = millis / 1000 in
    let nanos = (millis mod 1000) * 1_000_000 in
    { secs; nanos }
  let from_micros micros = 
    let secs = micros / 1_000_000 in
    let nanos = (micros mod 1_000_000) * 1_000 in
    { secs; nanos }
  let from_nanos nanos = 
    let secs = nanos / 1_000_000_000 in
    let remaining_nanos = nanos mod 1_000_000_000 in
    { secs; nanos = remaining_nanos }
  let from_secs_float f = 
    let secs = int_of_float f in
    let nanos = int_of_float ((f -. float_of_int secs) *. 1_000_000_000.0) in
    { secs; nanos }
  let from_weeks weeks = { secs = weeks * 604800; nanos = 0 }
  
  let to_secs t = t.secs
  let to_secs_float t = float_of_int t.secs +. (float_of_int t.nanos /. 1_000_000_000.0)
  let to_millis t = t.secs * 1000 + (t.nanos / 1_000_000)
  let to_micros t = t.secs * 1_000_000 + (t.nanos / 1_000)
  let to_ms = to_millis  (* Alias for compatibility *)
end

(**
  A measurement of a monotonically nondecreasing clock. Opaque and useful only with Duration.

Instants are always guaranteed, barring platform bugs, to be no less than any previously measured instant when created, and are often useful for tasks such as measuring benchmarks or timing how long an operation takes.

Note, however, that instants are not guaranteed to be steady. In other words, each tick of the underlying clock might not be the same length (e.g. some seconds may be longer than others). An instant may jump forwards or experience time dilation (slow down or speed up), but it will never go backwards. As part of this non-guarantee it is also not specified whether system suspends count as elapsed time or not. The behavior varies across platforms and Rust versions.

Instants are opaque types that can only be compared to one another. There is no method to get “the number of seconds” from an instant. Instead, it only allows measuring the duration between two instants (or comparing two instants).

The size of an Instant struct may vary depending on the target operating system.
*)
module Instant = struct
  type t = timespec

  (** TODO: The following system calls should be used by now() and implemented in `Std_sys.Time.Instant`, to find out the current time:

Platform	System call
SGX	insecure_time usercall. More information on timekeeping in SGX
UNIX	clock_gettime (Monotonic Clock)
Darwin	clock_gettime (Monotonic Clock)
VXWorks	clock_gettime (Monotonic Clock)
SOLID	get_tim
WASI	__wasi_clock_time_get (Monotonic Clock)
Windows	QueryPerformanceCounter
Disclaimer: These system calls might change over time.

Note: mathematical operations like add may panic if the underlying structure cannot represent the new point in time. *)
  let now () = 
    let time = Unix.gettimeofday () in
    let secs = int_of_float time in
    let nanos = int_of_float ((time -. float_of_int secs) *. 1_000_000_000.0) in
    { secs; nanos }
  
  let duration_since ~earlier later = 
    let secs_diff = later.secs - earlier.secs in
    let nanos_diff = later.nanos - earlier.nanos in
    if nanos_diff < 0 then
      { secs = secs_diff - 1; nanos = nanos_diff + 1_000_000_000 }
    else
      { secs = secs_diff; nanos = nanos_diff }
  
  let elapsed t = 
    duration_since ~earlier:t (now ())
end

(** An instant as recorded by the operating system, but different from [Instant.t] in that it is not guaranteed to be monotonic *)
module SystemTime = struct
  type t = timespec

  let now () = 
    let time = Unix.gettimeofday () in
    let secs = int_of_float time in
    let nanos = int_of_float ((time -. float_of_int secs) *. 1_000_000_000.0) in
    { secs; nanos }
  
  let duration_since ~earlier later = 
    let secs_diff = later.secs - earlier.secs in
    let nanos_diff = later.nanos - earlier.nanos in
    if nanos_diff < 0 then
      { secs = secs_diff - 1; nanos = nanos_diff + 1_000_000_000 }
    else
      { secs = secs_diff; nanos = nanos_diff }
  
  let elapsed t = 
    duration_since ~earlier:t (now ())
end






