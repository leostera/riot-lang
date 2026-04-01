open Global
(** Pure Gregorian calendar computations *)
(** {1 Types} *)

type year = int

type month = int

type day = int

type hour = int

type minute = int

type second = int

type day_number = int

type last_day_of_month = int

type week_number = int

type date = {
  year: year;
  month: month;
  day: day;
}

type time = {
  hour: hour;
  minute: minute;
  second: second;
}

type year_and_week = {
  year: year;
  week: week_number;
}

(** {1 Constants} *)

let seconds_per_minute = 60

let seconds_per_hour = 3_600

let seconds_per_day = 86_400

let days_per_year = 365

let days_per_leap_year = 366

let days_from_0_to_1970 = 719_528

let days_from_0_to_10000 = 2_932_897

let seconds_from_0_to_1970 = days_from_0_to_1970 * seconds_per_day

(** {1 Helper Functions} *)
(** Days in previous years (from Erlang calendar.erl) *)
let dy = fun year ->
  if year <= 0 then
    0
  else
    let x = year - 1 in
    (x / 4) - (x / 100) + (x / 400) + (x * days_per_year) + days_per_leap_year
(** Days in previous months (January = 1 .. December = 12) *)
let dm = function
  | 1 -> 0
  | 2 -> 31
  | 3 -> 59
  | 4 -> 90
  | 5 -> 120
  | 6 -> 151
  | 7 -> 181
  | 8 -> 212
  | 9 -> 243
  | 10 -> 273
  | 11 -> 304
  | 12 -> 334
  | m -> panic ("dm: invalid month " ^ string_of_int m)
(** Leap year adjustment for months after February *)
let df = fun year month ->
  if month < 3 then
    0
  else if (year mod 4 = 0 && year mod 100 != 0) || year mod 400 = 0 then
    1
  else
    0

(** {1 Leap Years and Month Information} *)

let is_leap_year = fun year -> (year mod 4 = 0 && year mod 100 != 0) || year mod 400 = 0

let last_day_of_month = fun ~year ~month ->
  match month with
  | 1
  | 3
  | 5
  | 7
  | 8
  | 10
  | 12 -> 31
  | 4
  | 6
  | 9
  | 11 -> 30
  | 2 ->
      if is_leap_year year then
        29
      else
        28
  | m -> panic ("last_day_of_month: invalid month " ^ string_of_int m)

(** {1 Date Validation} *)

let is_valid_date = fun { year; month; day } ->
  if month < 1 || month > 12 then
    false
  else if day < 1 then
    false
  else
    day <= last_day_of_month ~year ~month

(* Internal helper for backwards compatibility during transition *)

let valid_date = fun year month day -> is_valid_date { year; month; day }

(** {1 Gregorian Days Conversions} *)

let date_to_gregorian_days = fun { year; month; day } ->
  if not (is_valid_date { year; month; day }) then
    panic
      ("date_to_gregorian_days: invalid date "
      ^ string_of_int year
      ^ "-"
      ^ string_of_int month
      ^ "-"
      ^ string_of_int day)
  else
    dy year + dm month + df year month + day - 1
(** Convert day-of-year to date within a year *)
let year_day_to_date = fun year day_of_year ->
  let extra_day =
    if is_leap_year year then
      1
    else
      0
  in
  let rec find_month m =
    let days_before =
      match m with
      | 1 -> 0
      | 2 -> 31
      | 3 -> 59 + extra_day
      | 4 -> 90 + extra_day
      | 5 -> 120 + extra_day
      | 6 -> 151 + extra_day
      | 7 -> 181 + extra_day
      | 8 -> 212 + extra_day
      | 9 -> 243 + extra_day
      | 10 -> 273 + extra_day
      | 11 -> 304 + extra_day
      | 12 -> 334 + extra_day
      | _ -> panic "year_day_to_date: invalid month"
    in
    let days_in_month =
      match m with
      | 1
      | 3
      | 5
      | 7
      | 8
      | 10
      | 12 -> 31
      | 4
      | 6
      | 9
      | 11 -> 30
      | 2 ->
          if is_leap_year year then
            29
          else
            28
      | _ -> panic "year_day_to_date: invalid month"
    in
    if day_of_year < days_before + days_in_month then
      (m, day_of_year - days_before + 1)
    else
      find_month (m + 1)
  in
  find_month 1
(** Convert gregorian days to year and day-of-year *)
let day_to_year = fun days ->
  if days < 0 then
    panic "day_to_year: negative days";
  let y_max = days / days_per_year in
  let y_min = days / days_per_leap_year in
  (* Binary search refinement *)
  let rec refine y_min y_max =
    if y_min >= y_max then
      (y_min, days - dy y_min)
    else
      let y_mid = (y_min + y_max) / 2 in
      let d_mid = dy y_mid in
      let mid_length =
        if is_leap_year y_mid then
          days_per_leap_year
        else
          days_per_year
      in
      if days < d_mid then
        refine y_min (y_mid - 1)
      else if days - d_mid >= mid_length then
        refine (y_mid + 1) y_max
      else
        (y_mid, days - d_mid)
  in
  refine y_min y_max

let gregorian_days_to_date = fun days ->
  let year, day_of_year = day_to_year days in
  let month, day = year_day_to_date year day_of_year in
  { year; month; day }

(** {1 Gregorian Seconds Conversions} *)

let naive_to_gregorian_seconds = fun date time ->
  let days = date_to_gregorian_days date in
  let secs = (time.hour * seconds_per_hour) + (time.minute * seconds_per_minute) + time.second in
  (days * seconds_per_day) + secs

let gregorian_seconds_to_naive = fun secs ->
  let days = secs / seconds_per_day in
  let remaining_secs = secs mod seconds_per_day in
  let date = gregorian_days_to_date days in
  let hour = remaining_secs / seconds_per_hour in
  let remaining = remaining_secs mod seconds_per_hour in
  let minute = remaining / seconds_per_minute in
  let second = remaining mod seconds_per_minute in
  (date, { hour; minute; second })

(** {1 Day of Week} *)

let day_of_week = fun ({ year; month; day } as date) ->
  if not (is_valid_date date) then
    panic
      ("day_of_week: invalid date "
      ^ string_of_int year
      ^ "-"
      ^ string_of_int month
      ^ "-"
      ^ string_of_int day)
  else
    let days = date_to_gregorian_days date in
    ((days + 5) mod 7) + 1

(** {1 ISO Week Number} *)
(** Get gregorian days for the Monday of ISO week 1 in the given year *)
let gregorian_days_of_iso_w01_1 = fun year ->
  let jan1_date = { year; month = 1; day = 1 } in
  let jan1 = date_to_gregorian_days jan1_date in
  let dow = day_of_week jan1_date in
  (* ISO week 1 is the week with the first Thursday
     If Jan 1 is Mon-Thu (1-4), week 1 starts on that week's Monday
     If Jan 1 is Fri-Sun (5-7), week 1 starts next Monday *)
  if dow <= 4 then
    jan1 - dow + 1
  else
    jan1 + 7 - dow + 1

let iso_week_number = fun ({ year; month; day } as date) ->
  if not (is_valid_date date) then
    panic
      ("iso_week_number: invalid date "
      ^ string_of_int year
      ^ "-"
      ^ string_of_int month
      ^ "-"
      ^ string_of_int day);
  let d = date_to_gregorian_days date in
  let w01_1_year = gregorian_days_of_iso_w01_1 year in
  let w01_1_next_year = gregorian_days_of_iso_w01_1 (year + 1) in
  if w01_1_year <= d && d < w01_1_next_year then
    { year; week = ((d - w01_1_year) / 7) + 1 }
  else if d < w01_1_year then
    let prev_week_num =
      match day_of_week { year = year - 1; month = 1; day = 1 } with
      | 4 -> 53
      | _ -> (
          match day_of_week { year = year - 1; month = 12; day = 31 } with
          | 4 -> 53
          | _ -> 52
        )
    in
    { year = year - 1; week = prev_week_num }
  else
    (* d >= w01_1_next_year *)
    (* Next year, week 01 *)
    { year = year + 1; week = 1 }

(** {1 Time Conversions} *)

let time_to_seconds = fun { hour; minute; second } ->
  (hour * seconds_per_hour) + (minute * seconds_per_minute) + second

let seconds_to_time = fun secs ->
  if secs < 0 || secs >= seconds_per_day then
    panic ("seconds_to_time: seconds must be 0-86399, got " ^ string_of_int secs);
  let hour = secs / seconds_per_hour in
  let remaining = secs mod seconds_per_hour in
  let minute = remaining / seconds_per_minute in
  let second = remaining mod seconds_per_minute in
  { hour; minute; second }

let seconds_to_daystime = fun secs ->
  let days = secs / seconds_per_day in
  let remaining = secs mod seconds_per_day in
  if remaining < 0 then
    (days - 1, seconds_to_time (remaining + seconds_per_day))
  else
    (days, seconds_to_time remaining)

(** {1 Date/Time Arithmetic} *)

let time_difference = fun date1 time1 date2 time2 ->
  let secs1 = naive_to_gregorian_seconds date1 time1 in
  let secs2 = naive_to_gregorian_seconds date2 time2 in
  seconds_to_daystime (secs2 - secs1)
