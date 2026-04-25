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

(**
   Use `gettimeofday ()` for wall-clock seconds since the Unix epoch.

   This is for calendar conversion and timestamping, not elapsed-time measurement. Use
   [Monotonic] for durations and scheduler timing. 
*)
val gettimeofday: unit -> float

(** Use `localtime unix_time` to break a Unix timestamp into local calendar fields. *)
val localtime: float -> tm

(** Use `gmtime unix_time` to break a Unix timestamp into UTC calendar fields. *)
val gmtime: float -> tm

(**
   Use `mktime tm` to convert local calendar fields back into a Unix timestamp.

   The returned pair includes the normalized `tm` produced by the platform. 
*)
val mktime: tm -> float * tm

module SystemTime = System_time

module Monotonic = Monotonic

module Timer = Timer
