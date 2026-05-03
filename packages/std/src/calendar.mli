(**
   Pure Gregorian calendar computations.

   Pure mathematical functions for working with the Gregorian calendar.
   Provides date validation, conversions, day-of-week calculations, ISO week
   numbers, and date arithmetic without depending on system time.

   ## Design Philosophy

   Calendar provides **pure computations** independent of system time:
   - No system calls
   - Algorithmic date mathematics
   - Gregorian calendar rules
   - Suitable for date arithmetic and validation

   For current time operations, see {!Std.DateTime}.

   ## Quick Start

   ```ocaml
   open Std

   (* Validate dates *)
   let valid = Calendar.valid_date 2024 2 29 in  (* true - leap year *)
   let invalid = Calendar.valid_date 2023 2 29 in  (* false *)

   (* Get day of week *)
   let thursday = Calendar.day_of_the_week 2024 11 21 in  (* 4 = Thursday *)

   (* ISO week number *)
   let week = Calendar.iso_week_number {year=2024; month=11; day=21} in
   (* {year=2024; week=47} *)

   (* Date arithmetic using Gregorian days *)
   let days1 = Calendar.date_to_gregorian_days 2024 1 1 in
   let days2 = Calendar.date_to_gregorian_days 2024 12 31 in
   let days_between = days2 - days1 in  (* 365 *)
   ```

   ## The Gregorian Calendar

   All dates conform to the Gregorian calendar, extended back to year 0.
   This calendar was introduced by Pope Gregory XIII in 1582.

   ### Leap Year Rules

   A year Y is a leap year if:
   - Y is divisible by 4, but not by 100, OR
   - Y is divisible by 400

   Examples:
   - 1996 is a leap year (divisible by 4, not 100)
   - 1900 is not a leap year (divisible by 100, not 400)
   - 2000 is a leap year (divisible by 400)

   ## Gregorian Days and Seconds

   For date arithmetic and comparisons, dates can be converted to:
   - **Gregorian days**: Number of days since year 0, January 1st
   - **Gregorian seconds**: Number of seconds since year 0, midnight

   These provide a linear timeline for calculations.

   ## ISO 8601 Week Numbering

   Week numbers follow ISO 8601:
   - Week 1 is the week containing the first Thursday of the year
   - Weeks start on Monday (day 1)
   - Weeks can span year boundaries
   - Week numbers range from 1 to 53

   ## See Also

   - {!Std.DateTime} - Current time and timezone operations
   - {!Std.Time.Duration} - Time spans
   - {!Std.Time.SystemTime} - System clock measurements
*)
open Global

(** Day of week: 1 (Monday) .. 7 (Sunday), following ISO 8601 *)
type weekday =
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
type month =
  | January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
(** A date without timezone information *)
type date = { year: int; month: int; day: int }
(** A time without date or timezone *)
type time = { hour: int; minute: int; second: int }
(** Year and ISO week number *)
type year_and_week = { year: int; week: int }

(** 60 seconds per minute *)
val seconds_per_minute: int

(** 3600 seconds per hour *)
val seconds_per_hour: int

(** 86400 seconds per day *)
val seconds_per_day: int

(** 365 days per ordinary year *)
val days_per_year: int

(** 366 days per leap year *)
val days_per_leap_year: int

(** 719528 days from year 0 to Unix epoch (1970-01-01) *)
val days_from_0_to_1970: int

(**
   Returns [true] if the year is a leap year.

   A year is a leap year if:
   - It's divisible by 4 but not by 100, OR
   - It's divisible by 400

   ## Examples

   ```ocaml
   Calendar.is_leap_year 2000;;  (* true - divisible by 400 *)
   Calendar.is_leap_year 1900;;  (* false - divisible by 100 but not 400 *)
   Calendar.is_leap_year 2024;;  (* true - divisible by 4, not 100 *)
   Calendar.is_leap_year 2023;;  (* false *)
   ```
*)
val is_leap_year: year:int -> bool

(**
   Returns the last day of the month (28, 29, 30, or 31).

   Accounts for leap years when computing February's last day.

   ## Examples

   ```ocaml
   Calendar.last_day_of_month ~year:2024 ~month:2;;  (* 29 - leap year *)
   Calendar.last_day_of_month ~year:2023 ~month:2;;  (* 28 *)
   Calendar.last_day_of_month ~year:2024 ~month:4;;  (* 30 - April *)
   Calendar.last_day_of_month ~year:2024 ~month:1;;  (* 31 - January *)
   ```

   @raise Invalid_argument if month is not in range 1-12
*)
val last_day_of_month: year:int -> month:int -> int

(**
   Validates if a date is valid in the Gregorian calendar.

   Checks:
   - Month is in range 1-12
   - Day is in valid range for the month
   - Accounts for leap years in February

   ## Examples

   ```ocaml
   Calendar.is_valid_date {year=2024; month=2; day=29};;   (* true - leap year *)
   Calendar.is_valid_date {year=2023; month=2; day=29};;   (* false - not a leap year *)
   Calendar.is_valid_date {year=2024; month=4; day=31};;   (* false - April has 30 days *)
   Calendar.is_valid_date {year=2024; month=13; day=1};;   (* false - invalid month *)
   Calendar.is_valid_date {year=2024; month=12; day=31};;  (* true *)
   ```
*)
val is_valid_date: date -> bool

(**
   Converts a date to the number of days since year 0, January 1st.

   This is the foundation for date arithmetic and comparisons.

   ## Examples

   ```ocaml
   Calendar.date_to_gregorian_days {year=0; month=1; day=1};;      (* 0 - year 0, Jan 1 *)
   Calendar.date_to_gregorian_days {year=1970; month=1; day=1};;   (* 719528 - Unix epoch *)
   Calendar.date_to_gregorian_days {year=2024; month=11; day=21};; (* some number of days *)
   ```

   @raise Invalid_argument if the date is invalid
*)
val date_to_gregorian_days: date -> int

(**
   Converts gregorian days back to a date.

   Inverse of {!date_to_gregorian_days}.

   ## Examples

   ```ocaml
   Calendar.gregorian_days_to_date 0;;
   (* {year=0; month=1; day=1} *)

   Calendar.gregorian_days_to_date 719528;;
   (* {year=1970; month=1; day=1} *)
   ```

   ## Round-trip Property

   ```ocaml
   let d = {year=2024; month=11; day=21} in
   gregorian_days_to_date (date_to_gregorian_days' d) = d
   (* true *)
   ```
*)
val gregorian_days_to_date: int -> date

(**
   Converts a date and time to seconds since year 0, midnight.

   Used for datetime arithmetic and comparisons. For working with
   {!DateTime.naive}, extract the fields first.

   ## Examples

   ```ocaml
   let date = {year=1970; month=1; day=1} in
   let time = {hour=0; minute=0; second=0} in
   Calendar.naive_to_gregorian_seconds date time;;
   (* 62167219200 - seconds from year 0 to Unix epoch *)
   ```
*)
val naive_to_gregorian_seconds: date -> time -> int

(**
   Converts gregorian seconds to a date and time.

   Inverse of {!naive_to_gregorian_seconds}.

   ## Examples

   ```ocaml
   Calendar.gregorian_seconds_to_naive 0;;
   (* ({year=0; month=1; day=1}, {hour=0; minute=0; second=0}) *)
   ```
*)
val gregorian_seconds_to_naive: int -> date * time

(**
   Returns the day of week: 1=Monday, 2=Tuesday, ..., 7=Sunday.

   Follows ISO 8601 convention where Monday is day 1.

   ## Examples

   ```ocaml
   Calendar.day_of_week {year=2024; month=11; day=21};;  (* 4 - Thursday *)
   Calendar.day_of_week {year=2024; month=11; day=25};;  (* 1 - Monday *)
   Calendar.day_of_week {year=2024; month=11; day=24};;  (* 7 - Sunday *)
   ```

   @raise Invalid_argument if the date is invalid
*)
val day_of_week: date -> weekday

(**
   Calculates the ISO 8601 week number.

   ISO 8601 week rules:
   - Week 1 is the week containing the first Thursday of the year
   - Equivalently, week 1 is the week with 4 or more days in the new year
   - Weeks start on Monday
   - The week can span year boundaries

   Returns [{year; week}] which may have a different year than the input date.

   ## Examples

   ```ocaml
   (* A Monday in mid-year *)
   Calendar.iso_week_number {year=2024; month=11; day=25};;
   (* {year=2024; week=48} *)

   (* Near year boundary - may belong to previous/next year's week *)
   Calendar.iso_week_number {year=2024; month=1; day=1};;
   (* Could be {year=2024; week=1} or {year=2023; week=52} *)
   ```

   @raise Invalid_argument if the date is invalid
*)
val iso_week_number: date -> year_and_week

(**
   Converts time to seconds since midnight (0-86399).

   ## Examples

   ```ocaml
   Calendar.time_to_seconds {hour=0; minute=0; second=0};;   (* 0 *)
   Calendar.time_to_seconds {hour=1; minute=0; second=0};;   (* 3600 *)
   Calendar.time_to_seconds {hour=23; minute=59; second=59};; (* 86399 *)
   ```
*)
val time_to_seconds: time -> int

(**
   Converts seconds since midnight to time.

   Seconds must be in range 0-86399.

   ## Examples

   ```ocaml
   Calendar.seconds_to_time 0;;     (* {hour=0; minute=0; second=0} *)
   Calendar.seconds_to_time 3661;;  (* {hour=1; minute=1; second=1} *)
   ```

   @raise Invalid_argument if seconds is not in range 0-86399
*)
val seconds_to_time: int -> time

(**
   Converts any number of seconds to [(days, time)].

   Handles negative seconds correctly. Time is always non-negative.

   ## Examples

   ```ocaml
   Calendar.seconds_to_daystime 0;;
   (* (0, {hour=0; minute=0; second=0}) *)

   Calendar.seconds_to_daystime 90000;;
   (* (1, {hour=1; minute=0; second=0}) - 1 day + 1 hour *)

   Calendar.seconds_to_daystime (-3600);;
   (* (-1, {hour=23; minute=0; second=0}) - -1 day + 23 hours *)
   ```
*)
val seconds_to_daystime: int -> int * time

(**
   Computes the difference between two date/time pairs.

   Returns [(days, time)] where:
   - [time] is always non-negative
   - [days] can be negative if [date1/time1 > date2/time2]

   The result represents: [(date2, time2) - (date1, time1)]

   ## Examples

   ```ocaml
   let date1 = {year=2024; month=1; day=1} in
   let time1 = {hour=12; minute=0; second=0} in
   let date2 = {year=2024; month=1; day=2} in
   let time2 = {hour=14; minute=30; second=0} in
   Calendar.time_difference date1 time1 date2 time2;;
   (* (1, {hour=2; minute=30; second=0}) - 1 day, 2.5 hours *)
   ```
*)
val time_difference: src_date:date -> src_time:time -> dst_date:date -> dst_time:time -> int * time
