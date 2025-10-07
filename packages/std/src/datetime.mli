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
