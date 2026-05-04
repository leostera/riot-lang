open Global

type t = Calendar.date = { year: int; month: int; day: int }

type error =
  | Invalid_format of string
  | Invalid_date of string

let is_valid = Calendar.is_valid_date

let pad2 = fun n ->
  if n < 10 then
    "0" ^ Int.to_string n
  else
    Int.to_string n

let pad_year = fun year ->
  let abs_year = Int.abs year in
  let year_str = Int.to_string abs_year in
  let padded =
    if String.length year_str >= 4 then
      year_str
    else
      String.make ~len:(4 - String.length year_str) ~char:'0' ^ year_str
  in
  if year < 0 then
    "-" ^ padded
  else
    padded

let to_iso8601_unchecked = fun { year; month; day } ->
  pad_year year ^ "-" ^ pad2 month ^ "-" ^ pad2 day

let make = fun ~year ~month ~day ->
  let date = { year; month; day } in
  if is_valid date then
    Ok date
  else
    Error (Invalid_date ("invalid civil date: " ^ to_iso8601_unchecked date))

let compare = fun left right ->
  Int.compare
    (Calendar.date_to_gregorian_days left)
    (Calendar.date_to_gregorian_days right)

let equal = fun left right ->
  match compare left right with
  | Order.EQ -> true
  | Order.LT
  | Order.GT -> false

let from_date_time: DateTime.t -> t = fun dt -> { year = dt.year; month = dt.month; day = dt.day }

let today = fun () ->
  DateTime.now ()
  |> from_date_time

let today_utc = fun () ->
  DateTime.now_utc ()
  |> from_date_time

let add_days = fun date days ->
  Calendar.gregorian_days_to_date
    (Calendar.date_to_gregorian_days date + days)

let diff_days = fun left right ->
  Calendar.date_to_gregorian_days left - Calendar.date_to_gregorian_days right

let day_of_week = Calendar.day_of_week

let day_of_year = fun ({ year; _ } as date) ->
  let first_day = { year; month = 1; day = 1 } in
  Calendar.date_to_gregorian_days date - Calendar.date_to_gregorian_days first_day + 1

let iso_week_number = Calendar.iso_week_number

let is_leap_year = fun { year; _ } -> Calendar.is_leap_year ~year

let days_in_month = fun { year; month; _ } -> Calendar.last_day_of_month ~year ~month

let beginning_of_month = fun { year; month; _ } -> { year; month; day = 1 }

let end_of_month = fun ({ year; month; _ } as date) -> {
  date with
  day = Calendar.last_day_of_month ~year ~month;
}

let to_gregorian_days = Calendar.date_to_gregorian_days

let from_gregorian_days = Calendar.gregorian_days_to_date

let to_iso8601 = to_iso8601_unchecked

let to_string = to_iso8601

let parse_signed_year = fun value ->
  match String.length value with
  | 0 -> Error (Invalid_format "expected year component")
  | _ -> (
      match Int.parse value with
      | Some year -> Ok year
      | None -> Error (Invalid_format ("invalid year component: " ^ value))
    )

let parse_2digit = fun label value ->
  if not (Int.equal (String.length value) 2) then
    Error (Invalid_format ("expected two-digit " ^ label))
  else
    (
      match Int.parse value with
      | Some parsed -> Ok parsed
      | None -> Error (Invalid_format ("invalid " ^ label ^ " component: " ^ value))
    )

let from_iso8601 = fun value ->
  match String.split ~by:"-" value with
  | [ year_str; month_str; day_str ] -> (
      match (parse_signed_year year_str, parse_2digit "month" month_str, parse_2digit "day" day_str) with
      | (Ok year, Ok month, Ok day) -> make ~year ~month ~day
      | (Error err, _, _)
      | (_, Error err, _)
      | (_, _, Error err) -> Error err
    )
  | [ ""; year_str; month_str; day_str ] -> (
      match (
        parse_signed_year ("-" ^ year_str),
        parse_2digit "month" month_str,
        parse_2digit "day" day_str
      ) with
      | (Ok year, Ok month, Ok day) -> make ~year ~month ~day
      | (Error err, _, _)
      | (_, Error err, _)
      | (_, _, Error err) -> Error err
    )
  | _ -> Error (Invalid_format ("expected YYYY-MM-DD date, got: " ^ value))

let to_calendar_date = fun date -> date

let from_calendar_date = fun date -> make ~year:date.year ~month:date.month ~day:date.day
