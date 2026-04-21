open Std

let equal_date = fun (left: Calendar.date) (right: Calendar.date) ->
  Int.equal left.year right.year && Int.equal left.month right.month && Int.equal left.day right.day

let equal_time = fun (left: Calendar.time) (right: Calendar.time) ->
  Int.equal left.hour right.hour
  && Int.equal left.minute right.minute
  && Int.equal left.second right.second

let make_date = fun ~year ~month ~day -> ({ year; month; day }: Calendar.date)

let make_time = fun ~hour ~minute ~second -> ({ hour; minute; second }: Calendar.time)

let test_is_leap_year_2000 = fun _ctx ->
  if Calendar.is_leap_year ~year:2_000 then
    Ok ()
  else
    Error "expected 2000 to be a leap year"

let test_is_leap_year_1900 = fun _ctx ->
  if not (Calendar.is_leap_year ~year:1_900) then
    Ok ()
  else
    Error "expected 1900 not to be a leap year"

let test_is_leap_year_2024 = fun _ctx ->
  if Calendar.is_leap_year ~year:2_024 then
    Ok ()
  else
    Error "expected 2024 to be a leap year"

let test_is_leap_year_2023 = fun _ctx ->
  if not (Calendar.is_leap_year ~year:2_023) then
    Ok ()
  else
    Error "expected 2023 not to be a leap year"

let test_last_day_of_month_feb_leap = fun _ctx ->
  if Int.equal (Calendar.last_day_of_month ~year:2_024 ~month:2) 29 then
    Ok ()
  else
    Error "expected February 2024 to have 29 days"

let test_last_day_of_month_feb_non_leap = fun _ctx ->
  if Int.equal (Calendar.last_day_of_month ~year:2_023 ~month:2) 28 then
    Ok ()
  else
    Error "expected February 2023 to have 28 days"

let test_last_day_of_month_30_day = fun _ctx ->
  if Int.equal (Calendar.last_day_of_month ~year:2_024 ~month:4) 30 then
    Ok ()
  else
    Error "expected April to have 30 days"

let test_last_day_of_month_invalid_0 = fun _ctx ->
  try
    let _ = Calendar.last_day_of_month ~year:2_024 ~month:0 in
    Error "expected invalid month 0 to raise"
  with
  | Failure _ -> Ok ()
  | Invalid_argument _ -> Ok ()

let test_last_day_of_month_invalid_13 = fun _ctx ->
  try
    let _ = Calendar.last_day_of_month ~year:2_024 ~month:13 in
    Error "expected invalid month 13 to raise"
  with
  | Failure _ -> Ok ()
  | Invalid_argument _ -> Ok ()

let test_is_valid_date_leap = fun _ctx ->
  if Calendar.is_valid_date (make_date ~year:2_024 ~month:2 ~day:29) then
    Ok ()
  else
    Error "expected 2024-02-29 to be valid"

let test_is_valid_date_non_leap = fun _ctx ->
  if not (Calendar.is_valid_date (make_date ~year:2_023 ~month:2 ~day:29)) then
    Ok ()
  else
    Error "expected 2023-02-29 to be invalid"

let test_is_valid_date_april_31 = fun _ctx ->
  if not (Calendar.is_valid_date (make_date ~year:2_024 ~month:4 ~day:31)) then
    Ok ()
  else
    Error "expected 2024-04-31 to be invalid"

let test_date_to_gregorian_days_origin = fun _ctx ->
  if Int.equal (Calendar.date_to_gregorian_days (make_date ~year:0 ~month:1 ~day:1)) 0 then
    Ok ()
  else
    Error "expected origin date to map to zero gregorian days"

let test_date_to_gregorian_days_epoch = fun _ctx ->
  if
    Int.equal (Calendar.date_to_gregorian_days (make_date ~year:1_970 ~month:1 ~day:1)) Calendar.days_from_0_to_1970
  then
    Ok ()
  else
    Error "expected 1970-01-01 gregorian days to match days_from_0_to_1970"

let test_gregorian_days_to_date_origin = fun _ctx ->
  let actual = Calendar.gregorian_days_to_date 0 in
  let expected = make_date ~year:0 ~month:1 ~day:1 in
  if equal_date actual expected then
    Ok ()
  else
    Error "expected gregorian day 0 to roundtrip to origin"

let test_gregorian_days_to_date_epoch = fun _ctx ->
  let actual = Calendar.gregorian_days_to_date Calendar.days_from_0_to_1970 in
  let expected = make_date ~year:1_970 ~month:1 ~day:1 in
  if equal_date actual expected then
    Ok ()
  else
    Error "expected unix epoch gregorian day to roundtrip"

let test_gregorian_days_roundtrip_leap = fun _ctx ->
  let date = make_date ~year:2_024 ~month:2 ~day:29 in
  if equal_date (Calendar.gregorian_days_to_date (Calendar.date_to_gregorian_days date)) date then
    Ok ()
  else
    Error "expected leap date gregorian-day roundtrip"

let test_gregorian_days_roundtrip_non_leap = fun _ctx ->
  let date = make_date ~year:2_023 ~month:11 ~day:21 in
  if equal_date (Calendar.gregorian_days_to_date (Calendar.date_to_gregorian_days date)) date then
    Ok ()
  else
    Error "expected non-leap date gregorian-day roundtrip"

let test_naive_to_gregorian_seconds_origin = fun _ctx ->
  let date = make_date ~year:0 ~month:1 ~day:1 in
  let time = make_time ~hour:0 ~minute:0 ~second:0 in
  if Int.equal (Calendar.naive_to_gregorian_seconds date time) 0 then
    Ok ()
  else
    Error "expected year 0 midnight to map to zero gregorian seconds"

let test_gregorian_seconds_to_naive_origin = fun _ctx ->
  let (date, time) = Calendar.gregorian_seconds_to_naive 0 in
  if
    equal_date date (make_date ~year:0 ~month:1 ~day:1)
    && equal_time time (make_time ~hour:0 ~minute:0 ~second:0)
  then
    Ok ()
  else
    Error "expected gregorian seconds origin to roundtrip to year 0 midnight"

let test_gregorian_seconds_roundtrip_midnight = fun _ctx ->
  let date = make_date ~year:2_024 ~month:11 ~day:21 in
  let time = make_time ~hour:0 ~minute:0 ~second:0 in
  let (actual_date, actual_time) = Calendar.gregorian_seconds_to_naive
    (Calendar.naive_to_gregorian_seconds date time) in
  if equal_date actual_date date && equal_time actual_time time then
    Ok ()
  else
    Error "expected midnight gregorian-seconds roundtrip"

let test_gregorian_seconds_roundtrip_non_midnight = fun _ctx ->
  let date = make_date ~year:2_024 ~month:11 ~day:21 in
  let time = make_time ~hour:14 ~minute:7 ~second:31 in
  let (actual_date, actual_time) = Calendar.gregorian_seconds_to_naive
    (Calendar.naive_to_gregorian_seconds date time) in
  if equal_date actual_date date && equal_time actual_time time then
    Ok ()
  else
    Error "expected non-midnight gregorian-seconds roundtrip"

let test_day_of_week_monday = fun _ctx ->
  if Calendar.day_of_week (make_date ~year:2_024 ~month:11 ~day:25) = Calendar.Monday then
    Ok ()
  else
    Error "expected 2024-11-25 to be Monday"

let test_day_of_week_sunday = fun _ctx ->
  if Calendar.day_of_week (make_date ~year:2_024 ~month:11 ~day:24) = Calendar.Sunday then
    Ok ()
  else
    Error "expected 2024-11-24 to be Sunday"

let test_iso_week_number_mid_year = fun _ctx ->
  let actual = Calendar.iso_week_number (make_date ~year:2_024 ~month:11 ~day:25) in
  if Int.equal actual.year 2_024 && Int.equal actual.week 48 then
    Ok ()
  else
    Error "expected 2024-11-25 to be ISO week 48 of 2024"

let test_iso_week_number_year_boundary = fun _ctx ->
  let actual = Calendar.iso_week_number (make_date ~year:2_021 ~month:1 ~day:1) in
  if Int.equal actual.year 2_020 && Int.equal actual.week 53 then
    Ok ()
  else
    Error "expected 2021-01-01 to belong to ISO week 53 of 2020"

let test_time_to_seconds = fun _ctx ->
  if Int.equal (Calendar.time_to_seconds (make_time ~hour:23 ~minute:59 ~second:59)) 86_399 then
    Ok ()
  else
    Error "expected 23:59:59 to equal 86399 seconds"

let test_seconds_to_time = fun _ctx ->
  let actual = Calendar.seconds_to_time 86_399 in
  if equal_time actual (make_time ~hour:23 ~minute:59 ~second:59) then
    Ok ()
  else
    Error "expected 86399 seconds to map to 23:59:59"

let tests =
  Test.[
    case "Calendar.is_leap_year 2000" test_is_leap_year_2000;
    case "Calendar.is_leap_year 1900" test_is_leap_year_1900;
    case "Calendar.is_leap_year 2024" test_is_leap_year_2024;
    case "Calendar.is_leap_year 2023" test_is_leap_year_2023;
    case "Calendar.last_day_of_month leap February" test_last_day_of_month_feb_leap;
    case "Calendar.last_day_of_month non-leap February" test_last_day_of_month_feb_non_leap;
    case "Calendar.last_day_of_month 30-day month" test_last_day_of_month_30_day;
    case "Calendar.last_day_of_month rejects month 0" test_last_day_of_month_invalid_0;
    case "Calendar.last_day_of_month rejects month 13" test_last_day_of_month_invalid_13;
    case "Calendar.is_valid_date accepts leap day" test_is_valid_date_leap;
    case "Calendar.is_valid_date rejects non-leap February 29" test_is_valid_date_non_leap;
    case "Calendar.is_valid_date rejects April 31" test_is_valid_date_april_31;
    case "Calendar.date_to_gregorian_days origin" test_date_to_gregorian_days_origin;
    case "Calendar.date_to_gregorian_days unix epoch" test_date_to_gregorian_days_epoch;
    case "Calendar.gregorian_days_to_date origin" test_gregorian_days_to_date_origin;
    case "Calendar.gregorian_days_to_date unix epoch" test_gregorian_days_to_date_epoch;
    case "Calendar gregorian-day roundtrip leap date" test_gregorian_days_roundtrip_leap;
    case "Calendar gregorian-day roundtrip non-leap date" test_gregorian_days_roundtrip_non_leap;
    case "Calendar.naive_to_gregorian_seconds origin" test_naive_to_gregorian_seconds_origin;
    case "Calendar.gregorian_seconds_to_naive origin" test_gregorian_seconds_to_naive_origin;
    case "Calendar gregorian-seconds roundtrip midnight" test_gregorian_seconds_roundtrip_midnight;
    case "Calendar gregorian-seconds roundtrip non-midnight" test_gregorian_seconds_roundtrip_non_midnight;
    case "Calendar.day_of_week Monday" test_day_of_week_monday;
    case "Calendar.day_of_week Sunday" test_day_of_week_sunday;
    case "Calendar.iso_week_number mid-year" test_iso_week_number_mid_year;
    case "Calendar.iso_week_number year boundary" test_iso_week_number_year_boundary;
    case "Calendar.time_to_seconds" test_time_to_seconds;
    case "Calendar.seconds_to_time" test_seconds_to_time;
  ]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name:"calendar" ~tests ~args) ~args:Env.args ()
