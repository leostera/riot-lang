(** Date and time utilities *)

module Tz = struct
  type t = Etc_UTC | Local

  let to_string = function Etc_UTC -> "UTC" | Local -> "Local"
end

type t = {
  microseconds : int * int; (* (microseconds, precision) e.g. (426822, 6) *)
  second : int;
  minute : int;
  hour : int;
  day : int;
  month : int;
  year : int;
  time_zone : Tz.t;
  utc_offset : int;
  std_offset : int;
}

let now () =
  let unix_time = Kernel.Time.gettimeofday () in
  let tm = Kernel.Time.localtime unix_time in
  let microseconds =
    let frac = unix_time -. floor unix_time in
    let micros = int_of_float (frac *. 1_000_000.0) in
    (micros, 6)
  in
  {
    microseconds;
    second = tm.tm_sec;
    minute = tm.tm_min;
    hour = tm.tm_hour;
    day = tm.tm_mday;
    month = tm.tm_mon + 1;
    (* Unix.tm has 0-11, we want 1-12 *)
    year = tm.tm_year + 1900;
    time_zone = Tz.Local;
    utc_offset = 0;
    (* TODO: calculate actual offset *)
    std_offset = 0;
  }

let now_utc () =
  let unix_time = Kernel.Time.gettimeofday () in
  let tm = Kernel.Time.gmtime unix_time in
  let microseconds =
    let frac = unix_time -. floor unix_time in
    let micros = int_of_float (frac *. 1_000_000.0) in
    (micros, 6)
  in
  {
    microseconds;
    second = tm.tm_sec;
    minute = tm.tm_min;
    hour = tm.tm_hour;
    day = tm.tm_mday;
    month = tm.tm_mon + 1;
    year = tm.tm_year + 1900;
    time_zone = Tz.Etc_UTC;
    utc_offset = 0;
    std_offset = 0;
  }

let from_unix_time unix_time =
  let tm = Kernel.Time.localtime unix_time in
  let microseconds =
    let frac = unix_time -. floor unix_time in
    let micros = int_of_float (frac *. 1_000_000.0) in
    (micros, 6)
  in
  {
    microseconds;
    second = tm.tm_sec;
    minute = tm.tm_min;
    hour = tm.tm_hour;
    day = tm.tm_mday;
    month = tm.tm_mon + 1;
    year = tm.tm_year + 1900;
    time_zone = Tz.Local;
    utc_offset = 0;
    std_offset = 0;
  }

let to_timestamp t =
  let tm =
    {
      Kernel.Time.tm_sec = t.second;
      tm_min = t.minute;
      tm_hour = t.hour;
      tm_mday = t.day;
      tm_mon = t.month - 1;
      tm_year = t.year - 1900;
      tm_wday = 0;
      tm_yday = 0;
      tm_isdst = false;
    }
  in
  let unix_time, _ = Kernel.Time.mktime tm in
  unix_time

let to_iso8601 t =
  let micros, _ = t.microseconds in
  let millis = micros / 1000 in
  let tz_suffix =
    match t.time_zone with
    | Tz.Etc_UTC -> "Z"
    | Tz.Local ->
        if t.utc_offset = 0 then "Z"
        else
          let hours = abs t.utc_offset / 3600 in
          let mins = abs t.utc_offset mod 3600 / 60 in
          let sign = if t.utc_offset >= 0 then "+" else "-" in
          format "%s%02d:%02d" sign hours mins
  in
  format "%04d-%02d-%02dT%02d:%02d:%02d.%03d%s" t.year t.month t.day t.hour
    t.minute t.second millis tz_suffix
