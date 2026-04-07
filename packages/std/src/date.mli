(** # Date - Civil Gregorian dates

    A date without time-of-day or timezone information.

    [Date] is the user-facing civil-date API layered on top of {!Std.Calendar}.
    Use it for calendar dates, day arithmetic, and ISO 8601 date parsing.

    ## Examples

    ```ocaml
    open Std

    let birthday = Date.make ~year:1988 ~month:5 ~day:17 |> Result.unwrap in
    let next_week = Date.add_days birthday 7 in

    let iso = Date.to_iso8601 birthday in
    (* "1988-05-17" *)
    ```

    ## See Also

    - {!Std.Calendar} for low-level Gregorian calendar computations
    - {!Std.DateTime} for timezone-aware calendar datetimes
    - {!Std.Time.SystemTime} for wall-clock timestamps *)

open Global

type t = Calendar.date = {
  year: Calendar.year;
  month: Calendar.month;
  day: Calendar.day;
}
type error =
  | Invalid_format of string
  | Invalid_date of string
val make: year:int -> month:int -> day:int -> (t, error) result

val is_valid: t -> bool

val compare: t -> t -> int

val equal: t -> t -> bool

val today: unit -> t

val today_utc: unit -> t

val add_days: t -> int -> t

val diff_days: t -> t -> int

val day_of_week: t -> Calendar.day_number

val day_of_year: t -> int

val iso_week_number: t -> Calendar.year_and_week

val is_leap_year: t -> bool

val days_in_month: t -> Calendar.last_day_of_month

val beginning_of_month: t -> t

val end_of_month: t -> t

val to_gregorian_days: t -> int

val of_gregorian_days: int -> t

val to_iso8601: t -> string

val to_string: t -> string

val of_iso8601: string -> (t, error) result

val of_date_time: DateTime.t -> t

val to_calendar_date: t -> Calendar.date

val of_calendar_date: Calendar.date -> (t, error) result
