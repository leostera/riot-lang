(** # DateTime - Calendar date and time operations

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
    - [Time.Duration] for time spans *)

(** {1 Time Zones} *)

module Tz : sig
  type t =
    | Etc_UTC
    | Local
        (** Time zone representation:
            - [Etc_UTC]: UTC/GMT timezone
            - [Local]: System's local timezone *)

  val to_string : t -> string
  (** Converts timezone to string representation.

      ## Examples

      ```ocaml Tz.to_string Tz.Etc_UTC (* "UTC" *) Tz.to_string Tz.Local (*
      "Local" *) ``` *)
end

(** {1 Types} *)

open Global

type t = {
  microseconds : int * int;
      (** Microseconds and precision, e.g. (426822, 6) means 426822 microseconds
          with 6 digits of precision *)
  second : int;  (** Second (0-59) *)
  minute : int;  (** Minute (0-59) *)
  hour : int;  (** Hour (0-23) *)
  day : int;  (** Day of month (1-31) *)
  month : int;  (** Month (1-12) *)
  year : int;  (** Year *)
  time_zone : Tz.t;  (** Time zone *)
  utc_offset : int;  (** UTC offset in seconds *)
  std_offset : int;  (** Standard time offset *)
}
(** A date and time with calendar fields and timezone information. *)

(** {1 Creation} *)

val now : unit -> t
(** Returns the current date and time in the system's local timezone.

    ## Examples

    ```ocaml let now = DateTime.now () in Printf.printf "%04d-%02d-%02d
    %02d:%02d:%02d\n" now.year now.month now.day now.hour now.minute now.second
    (* "2025-08-27 14:07:31" *) ``` *)

val now_utc : unit -> t
(** Returns the current date and time in UTC.

    ## Examples

    ```ocaml let utc = DateTime.now_utc () in assert (utc.time_zone =
    Tz.Etc_UTC); assert (utc.utc_offset = 0) ``` *)

val from_unix_time : float -> t
(** Creates a datetime from a Unix timestamp (seconds since epoch).

    ## Examples

    ```ocaml (* Unix epoch *) let epoch = DateTime.from_unix_time 0.0 in (*
    1970-01-01 00:00:00 UTC *)

    (* Specific timestamp *) let dt = DateTime.from_unix_time 1724789251.426822
    in dt.year (* 2025 *) ```

    ## Note

    The timestamp is interpreted in UTC. *)

(** {1 Conversion} *)

val to_timestamp : t -> float
(** Converts datetime to Unix timestamp (seconds since epoch).

    ## Examples

    ```ocaml let now = DateTime.now () in let ts = DateTime.to_timestamp now in
    (* 1724789251.426822 *)

    (* Round-trip conversion *) let dt' = DateTime.from_unix_time ts in dt'.year
    = now.year (* true *) ``` *)

val to_unix_micros : t -> int64
(** Converts datetime to int64 microseconds since Unix epoch.
    
    Provides exact 1μs precision suitable for LSM storage and high-precision
    timestamps. This is preferred over float timestamps when exact precision
    is required.
    
    ## Examples
    
    ```ocaml
    let now = DateTime.now_utc () in
    let micros = DateTime.to_unix_micros now in
    (* 1724789251426822L *)
    
    (* Round-trip *)
    let dt' = DateTime.from_unix_micros micros in
    dt'.microseconds = now.microseconds (* true *)
    ```
    
    ## Precision
    
    - Exact 1 microsecond resolution
    - Range: ±292,000 years from epoch
    - No float rounding errors
    
    @since 1.0.0
*)

val from_unix_micros : int64 -> t
(** Converts int64 microseconds since Unix epoch to datetime.
    
    ## Examples
    
    ```ocaml
    let micros = 1724789251426822L in
    let dt = DateTime.from_unix_micros micros in
    dt.year (* 2025 *)
    ```
    
    @since 1.0.0
*)

val to_iso8601 : t -> string
(** Converts to ISO 8601 format string with microsecond precision.

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
    - Interoperability with other systems *)

(** {1 Parsing} *)

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

val parse : string -> (t, error) result
(** Parses an ISO 8601 datetime string into a DateTime.

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

    (* Datetime with timezone offset *)
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
    - **Datetime separator**: Either `T` or space (` `)
    - **Decimal separator**: Either `.` or `,` for fractional seconds
    - **Timezone**: `Z` for UTC, or `±HH:MM` / `±HHMM` for offsets
    - **Microseconds**: Up to 6 decimal places supported

    ## Notes

    - Timezone offsets are converted to seconds and stored in utc_offset
    - Missing microseconds default to 0
    - The resulting datetime uses Tz.Local for offset times, Tz.Etc_UTC for "Z"
    - Leap year validation is performed for negative years
    - Compatible with Elixir's DateTime.from_iso8601/2 parser *)
