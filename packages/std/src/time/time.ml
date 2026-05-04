module Duration = Duration
module Instant = Instant
module SystemTime = System_time

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

let from_kernel_tm = fun (tm: Kernel.Time.tm) ->
  {
    tm_sec = tm.tm_sec;
    tm_min = tm.tm_min;
    tm_hour = tm.tm_hour;
    tm_mday = tm.tm_mday;
    tm_mon = tm.tm_mon;
    tm_year = tm.tm_year;
    tm_wday = tm.tm_wday;
    tm_yday = tm.tm_yday;
    tm_isdst = tm.tm_isdst;
  }

let to_kernel_tm = fun tm ->
  ({
    tm_sec = tm.tm_sec;
    tm_min = tm.tm_min;
    tm_hour = tm.tm_hour;
    tm_mday = tm.tm_mday;
    tm_mon = tm.tm_mon;
    tm_year = tm.tm_year;
    tm_wday = tm.tm_wday;
    tm_yday = tm.tm_yday;
    tm_isdst = tm.tm_isdst;
  }: Kernel.Time.tm)

let localtime = fun timestamp -> from_kernel_tm (Kernel.Time.localtime timestamp)

let gmtime = fun timestamp -> from_kernel_tm (Kernel.Time.gmtime timestamp)

let mktime = fun tm ->
  let (timestamp, normalized) = Kernel.Time.mktime (to_kernel_tm tm) in
  (timestamp, from_kernel_tm normalized)
