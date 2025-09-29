(** An instant as recorded by the operating system, but different from
    [Instant.t] in that it is not guaranteed to be monotonic.

    NOTE: Platform-specific behavior The precision of SystemTime can depend on
    the underlying OS-specific time format. For example, on Windows the time is
    represented in 100 nanosecond intervals whereas Linux can represent
    nanosecond intervals.

    The following system calls are currently being used by now() to find out the
    current time:

    Platform System call SGX insecure_time usercall. More information on
    timekeeping in SGX UNIX clock_gettime (Realtime Clock) Darwin clock_gettime
    (Realtime Clock) VXWorks clock_gettime (Realtime Clock) SOLID
    SOLID_RTC_ReadTime WASI __wasi_clock_time_get (Realtime Clock) Windows
    GetSystemTimePreciseAsFileTime / GetSystemTimeAsFileTime *)

type t

(** {1 Creation} *)

val now : unit -> t
(** Returns the current system time *)

(** {1 Duration Operations} *)

val duration_since : earlier:t -> t -> Duration.t
(** Returns the amount of time elapsed between two system times *)

val elapsed : t -> Duration.t
(** Time elapsed since this system time *)

(** {1 Arithmetic Operations} *)

val add : t -> Duration.t -> t
(** Add a duration to a system time *)

val sub : t -> Duration.t -> t
(** Subtract a duration from a system time *)

(** {1 Checked Operations} *)

val checked_add : t -> Duration.t -> t option
(** Returns [Some result] if the result can be represented, [None] otherwise *)

val checked_sub : t -> Duration.t -> t option
(** Returns [Some result] if the result can be represented, [None] otherwise *)

(** {1 Comparison} *)

val compare : t -> t -> int
(** Compare two system times *)

val equal : t -> t -> bool
(** Test equality of two system times *)

val min : t -> t -> t
val max : t -> t -> t
