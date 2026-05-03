(**
   Calendar date and time operations.

   Date and time utilities for working with calendar dates, timestamps, and
   time zones. Provides conversions between different representations.

   ## Examples

   Getting current time:

   ```ocaml open Std

   (* Local time *) let now = DateTime.now () in Log.info "Current time:
   %02d:%02d:%02d" now.hour now.minute now.second

   (* UTC time *) let utc = DateTime.now_utc () in Log.info "UTC: %s"
   (DateTime.to_iso8601 utc) (* "2025-08-27T21:07:31.426Z" *) ```

   Working with timestamps:

   ```ocaml (* Convert to Unix timestamp *) let now = DateTime.now () in let
   timestamp = DateTime.to_timestamp now in (* 1724789251.426 *)

   (* Create from Unix timestamp *) let dt = DateTime.from_unix_time
   1724789251.426 in Printf.printf "%04d-%02d-%02d" dt.year dt.month dt.day ```

   ISO 8601 formatting:

   ```ocaml let utc = DateTime.now_utc () in let iso = DateTime.to_iso8601 utc
   in (* "2025-08-27T21:07:31.426822Z" *)

   (* Can be logged or stored *) Log.info "Event timestamp: %s" iso ```

   ## Time Zones

   The module supports UTC and local time zones:

   ```ocaml let local = DateTime.now () in let utc = DateTime.now_utc () in

   match local.time_zone with | Tz.Local -> Printf.printf "UTC offset: %d
   seconds" local.utc_offset | Tz.Etc_UTC -> Printf.printf "Already UTC" ```

   ## Differences from SystemTime

   | DateTime | SystemTime | |----------|------------| | Calendar dates |
   Opaque time point | | Human-readable fields | No calendar conversion | | ISO
   8601 formatting | No formatting | | Time zone aware | Time zone agnostic | |
   Suitable for logging | Suitable for durations |

   ## See Also

   - [Time.SystemTime] for system clock measurements
   - [Time.Instant] for monotonic time measurements
   - [Time.Duration] for time spans
*)
module Tz: sig
  type t =
    | Etc_UTC
    | Local

  val to_string: t -> string

  (**
     Converts timezone to string representation.

     ## Examples

     ```ocaml Tz.to_string Tz.Etc_UTC (* "UTC" *) Tz.to_string Tz.Local (*
     "Local" *) ```
  *)
end

open Global

type t = {
  microseconds: int * int;
  (**
     Microseconds and precision, e.g. (426822, 6) means 426822 microseconds
     with 6 digits of precision
  *)
  second: int;
  (** Second (0-59) *)
  minute: int;
  (** Minute (0-59) *)
  hour: int;
  (** Hour (0-23) *)
  day: int;
  (** Day of month (1-31) *)
  month: int;
  (** Month (1-12) *)
  year: int;
  (** Year *)
  time_zone: Tz.t;
  (** Time zone *)
  utc_offset: int;
  (** UTC offset in seconds *)
  std_offset: int;
  (** Standard time offset *)
}
(** A date and time with calendar fields and timezone information. *)
type naive = {
  year: int;
  (** Year *)
  month: int;
  (** Month (1-12) *)
  day: int;
  (** Day of month (1-31) *)
  hour: int;
  (** Hour (0-23) *)
  minute: int;
  (** Minute (0-59) *)
  second: int;
  (** Second (0-59) *)
  microsecond: int;
  (** Microseconds (0-999999) *)
}

(**
   A naive datetime without timezone information.

   Naive datetimes represent a date and time without any timezone context.
   They are useful for:
   - Pure calendar computations
   - When timezone is implicit or doesn't matter
   - Storing local times without conversion

   To work with actual wall-clock times, convert to {!t} using {!from_naive}.
*)
val epoch: t

(**
   The Unix epoch: January 1, 1970 00:00:00 UTC.

   ## Examples

   ```ocaml
   let epoch = DateTime.epoch in
   (* epoch.year = 1970, epoch.month = 1, epoch.day = 1 *)
   (* epoch.hour = 0, epoch.minute = 0, epoch.second = 0 *)
   ```
*)
val now: unit -> t

(**
   Returns the current date and time in the system's local timezone.

   ## Examples

   ```ocaml let now = DateTime.now () in Printf.printf "%04d-%02d-%02d
   %02d:%02d:%02d\n" now.year now.month now.day now.hour now.minute now.second
   (* "2025-08-27 14:07:31" *) ```
*)
val now_utc: unit -> t

(**
   Returns the current date and time in UTC.

   ## Examples

   ```ocaml let utc = DateTime.now_utc () in assert (utc.time_zone =
   Tz.Etc_UTC); assert (utc.utc_offset = 0) ```
*)
val now_naive: unit -> naive

(**
   Returns the current date and time as a naive datetime (no timezone).

   This is equivalent to [to_naive (now ())] but more convenient.
   Use this when you want the current local time without timezone context.

   ## Examples

   ```ocaml
   let naive_now = DateTime.now_naive () in
   Printf.printf "%04d-%02d-%02d %02d:%02d:%02d\n"
     naive_now.year naive_now.month naive_now.day
     naive_now.hour naive_now.minute naive_now.second
   (* "2025-11-21 14:30:45" *)
   ```

   ## Use Cases

   - Logging timestamps without timezone information
   - Storing local times in databases
   - Display times that don't need timezone conversion
*)
val from_system_time: Time.SystemTime.t -> t

(**
   Creates a datetime from a system time.

   ## Examples

   ```ocaml
   (* From current system time *)
   let sys_time = Time.SystemTime.now () in
   let dt = DateTime.from_system_time sys_time

   (* From Unix timestamp *)
   let sys_time = Time.SystemTime.from_unix_timestamp 1724789251 in
   let dt = DateTime.from_system_time sys_time
   (* dt represents 2025-08-27 21:07:31 UTC *)
   ```

   ## Note

   The resulting datetime is in UTC timezone.
*)
val to_system_time: t -> Time.SystemTime.t

(**
   Converts datetime to system time.

   ## Examples

   ```ocaml
   let dt = DateTime.now () in
   let sys_time = DateTime.to_system_time dt in
   let timestamp = Time.SystemTime.to_unix_timestamp sys_time
   ```

   ## Note

   Timezone information is preserved during conversion.
*)
val to_naive: t -> naive

(**
   Converts a timezone-aware datetime to a naive datetime by dropping timezone information.

   The resulting naive datetime represents the same calendar date and time,
   but without any timezone context. Microseconds are preserved.

   ## Examples

   ```ocaml
   let utc = DateTime.now_utc () in
   let naive = DateTime.to_naive utc in
   (* naive.year = utc.year, naive.hour = utc.hour, etc. *)
   (* But naive has no time_zone field *)
   ```

   ## Use Cases

   - Storing local times without timezone conversion
   - Calendar computations that don't depend on timezone
   - Displaying times without timezone context
*)
val from_naive: naive -> tz:Tz.t -> t

(**
   Converts a naive datetime to a timezone-aware datetime.

   Interprets the naive datetime in the specified timezone. The calendar
   fields remain the same, but timezone information is added.

   ## Examples

   ```ocaml
   let naive = {
     year = 2025; month = 1; day = 15;
     hour = 14; minute = 30; second = 0;
     microsecond = 0;
   } in

   (* Interpret as UTC *)
   let utc = DateTime.from_naive naive ~tz:Tz.Etc_UTC in
   (* utc.year = 2025, utc.time_zone = Etc_UTC *)

   (* Interpret as local time *)
   let local = DateTime.from_naive naive ~tz:Tz.Local in
   (* local.year = 2025, local.time_zone = Local *)
   ```

   ## Note

   When using [Tz.Local], the UTC offset is determined by the system's
   current timezone setting at the time of conversion.
*)
val to_iso8601: t -> string

(**
   Converts to ISO 8601 format string with microsecond precision.

   ## Examples

   ```ocaml let utc = DateTime.now_utc () in DateTime.to_iso8601 utc (*
   "2025-08-27T21:07:31.426822Z" *)

   let local = DateTime.now () in DateTime.to_iso8601 local (*
   "2025-08-27T14:07:31.426822-07:00" (with timezone offset) *) ```

   ## Format

   The format follows ISO 8601:
   - UTC times end with "Z"
   - Local times include timezone offset (e.g. "-07:00", "+05:30")
   - Microseconds are included with up to 6 decimal places

   ## Use Cases

   - Logging timestamps
   - Storing dates in databases
   - API responses
   - Interoperability with other systems
*)
val equal: t -> t -> bool

(**
   Tests equality of two datetimes.

   Two datetimes are equal if they represent the same point in time,
   comparing their system time representation at nanosecond precision.

   ## Examples

   ```ocaml
   let dt1 = DateTime.now () in
   let dt2 = DateTime.now () in
   DateTime.equal dt1 dt1 (* true *)
   DateTime.equal dt1 dt2 (* likely false - different times *)
   ```

   ## Note

   This compares the actual point in time, taking timezones into account.
   Two datetimes with different timezone offsets but representing the
   same moment will be equal.
*)
type error =
  | Invalid_format of string
  (** The input string doesn't match expected ISO 8601 format *)
  | Invalid_date of string
  (** Date components are invalid (e.g., February 30th) *)
  | Invalid_time of string
  (** Time components are invalid (e.g., 25:00:00) *)
  | Invalid_timezone of string

(** Timezone offset is malformed *)

(** Errors that can occur when parsing datetime strings. *)
val parse: string -> (t, error) result

(**
   Parses an ISO 8601 datetime string into a DateTime.

   This function has full parity with Elixir's DateTime.from_iso8601/2 parser.

   ## Examples

   ```ocaml (* UTC datetime with microseconds *)
   match DateTime.parse "2025-08-27T14:07:31.426822Z" with
   | Ok dt -> Printf.printf "Year: %d\n" dt.year
   | Error err -> Printf.printf "Parse error: %s\n" (match err with
       | Invalid_format msg -> "Invalid format: " ^ msg
       | Invalid_date msg -> "Invalid date: " ^ msg
       | Invalid_time msg -> "Invalid time: " ^ msg
       | Invalid_timezone msg -> "Invalid timezone: " ^ msg)

   (* DateTime with timezone offset *)
   let dt = DateTime.parse "2025-08-27T14:07:31+05:30" |> Result.unwrap in
   (* dt.time_zone = Tz.Local, dt.utc_offset = 19800 *)

   (* Space as separator *)
   let dt = DateTime.parse "2025-08-27 14:07:31Z" |> Result.unwrap in

   (* Basic format (no separators) *)
   let dt = DateTime.parse "20250827T140731Z" |> Result.unwrap in

   (* Comma as decimal separator *)
   let dt = DateTime.parse "2025-08-27T14:07:31,426Z" |> Result.unwrap in

   (* Negative year *)
   let dt = DateTime.parse "-2015-08-27T14:07:31Z" |> Result.unwrap in
   (* dt.year = -2015 *) ```

   ## Supported Formats

   The function supports both **extended** and **basic** ISO 8601 formats:

   ### Extended Format (with separators):
   - `YYYY-MM-DDTHH:MM:SSZ` (UTC)
   - `YYYY-MM-DD HH:MM:SSZ` (space separator)
   - `YYYY-MM-DDTHH:MM:SS±HH:MM` (with timezone offset)
   - `YYYY-MM-DDTHH:MM:SS.ssssssZ` (with microseconds, dot separator)
   - `YYYY-MM-DDTHH:MM:SS,ssssssZ` (with microseconds, comma separator)
   - `-YYYY-MM-DDTHH:MM:SSZ` (negative year)
   - `+YYYY-MM-DDTHH:MM:SSZ` (explicit positive year)

   ### Basic Format (no separators):
   - `YYYYMMDDTHHMMSSZ` (UTC)
   - `YYYYMMDD HHMMSSZ` (space separator)
   - `YYYYMMDDTHHMMSS±HHMM` (with timezone offset)
   - `YYYYMMDDTHHMMSS.ssssssZ` (with microseconds)
   - `-YYYYMMDDTHHMMSSZ` (negative year)

   ## Format Details

   - **Date separators**: Extended format uses `-`, basic format uses none
   - **Time separators**: Extended format uses `:`, basic format uses none
   - **DateTime separator**: Either `T` or space (` `)
   - **Decimal separator**: Either `.` or `,` for fractional seconds
   - **Timezone**: `Z` for UTC, or `±HH:MM` / `±HHMM` for offsets
   - **Microseconds**: Up to 6 decimal places supported

   ## Notes

   - Timezone offsets are converted to seconds and stored in utc_offset
   - Missing microseconds default to 0
   - The resulting datetime uses Tz.Local for offset times, Tz.Etc_UTC for "Z"
   - Leap year validation is performed for negative years
   - Compatible with Elixir's DateTime.from_iso8601/2 parser
*)
