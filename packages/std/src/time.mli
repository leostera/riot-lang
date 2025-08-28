(** 
A Duration type to represent a span of time, typically used for system timeouts.

Each Duration is composed of a whole number of seconds and a fractional part represented in nanoseconds. If the underlying system does not support nanosecond-level precision, APIs binding a system timeout will typically round up the number of nanoseconds.

Durations implement many common traits, including Add, Sub, and other ops traits. It implements Default by returning a zero-length Duration.
*)
module Duration : sig
  type t

  val is_zero : t -> bool
  val make : secs:int -> nanos:int -> t

  val abs_diff: t -> t -> t

  val from_days : int -> t
  val from_hours : int -> t
  val from_micros : int -> t
  val from_millis : int -> t
  val from_mins : int -> t
  val from_nanos : int -> t
  val from_secs : int -> t
  val from_secs_float : float -> t
  val from_weeks : int -> t

  val to_secs : t -> int
  val to_secs_float : t -> float
  val to_millis : t -> int
  val to_micros : t -> int
end

(**
  A measurement of a monotonically nondecreasing clock. Opaque and useful only with Duration.

Instants are always guaranteed, barring platform bugs, to be no less than any previously measured instant when created, and are often useful for tasks such as measuring benchmarks or timing how long an operation takes.

Note, however, that instants are not guaranteed to be steady. In other words, each tick of the underlying clock might not be the same length (e.g. some seconds may be longer than others). An instant may jump forwards or experience time dilation (slow down or speed up), but it will never go backwards. As part of this non-guarantee it is also not specified whether system suspends count as elapsed time or not. The behavior varies across platforms and Rust versions.

Instants are opaque types that can only be compared to one another. There is no method to get “the number of seconds” from an instant. Instead, it only allows measuring the duration between two instants (or comparing two instants).

The size of an Instant struct may vary depending on the target operating system.
*)
module Instant : sig
  type t

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
  val now : unit -> t

  val duration_since : earlier:t -> t -> Duration.t

  (** Time elapsed since [t], represented as a Duration.t *)
  val elapsed : t -> Duration.t

end

(** An instant as recorded by the operating system, but different from [Instant.t] in that it is not guaranteed to be monotonic.

NOTE: Platform-specific behavior
The precision of SystemTime can depend on the underlying OS-specific time format. For example, on Windows the time is represented in 100 nanosecond intervals whereas Linux can represent nanosecond intervals.

The following system calls are currently being used by now() to find out the current time:

Platform	System call
SGX	insecure_time usercall. More information on timekeeping in SGX
UNIX	clock_gettime (Realtime Clock)
Darwin	clock_gettime (Realtime Clock)
VXWorks	clock_gettime (Realtime Clock)
SOLID	SOLID_RTC_ReadTime
WASI	__wasi_clock_time_get (Realtime Clock)
Windows	GetSystemTimePreciseAsFileTime / GetSystemTimeAsFileTime

    *)
module SystemTime : sig
  type t
  val now : unit -> t

  val duration_since : earlier:t -> t -> Duration.t

  (** Time elapsed since [t], represented as a Duration.t *)
  val elapsed : t -> Duration.t

end
