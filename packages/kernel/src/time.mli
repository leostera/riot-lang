(** Low-level time operations for Kernel - minimal interface *)

type tm = {
  tm_sec : int;  (** Seconds 0-60 *)
  tm_min : int;  (** Minutes 0-59 *)
  tm_hour : int;  (** Hours 0-23 *)
  tm_mday : int;  (** Day of month 1-31 *)
  tm_mon : int;  (** Month 0-11 *)
  tm_year : int;  (** Year - 1900 *)
  tm_wday : int;  (** Day of week 0-6 (Sunday = 0) *)
  tm_yday : int;  (** Day of year 0-365 *)
  tm_isdst : bool;  (** Daylight saving time *)
}
(** Broken-down time representation compatible with C's struct tm *)

val gettimeofday : unit -> float
(** Get current time since Unix epoch with microsecond precision *)

val localtime : float -> tm
(** Convert Unix timestamp to broken-down time in local timezone *)

val gmtime : float -> tm
(** Convert Unix timestamp to broken-down time in UTC *)

val mktime : tm -> float * tm
(** Convert broken-down time to Unix timestamp. Returns (timestamp,
    normalized_tm) *)

val monotonic_time_nanos : unit -> int64
(** Get monotonic time in nanoseconds. This clock is immune to system clock
    adjustments and is suitable for measuring elapsed time. Uses CLOCK_MONOTONIC
    on Linux, mach_absolute_time on macOS. *)

val sleep : float -> unit
(** Sleep for the specified number of seconds *)
