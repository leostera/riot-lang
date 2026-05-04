open Std

let test_make_valid_date = fun _ctx ->
  match Date.make ~year:2_024 ~month:2 ~day:29 with
  | Ok date when date.year = 2_024 && date.month = 2 && date.day = 29 -> Ok ()
  | Ok _ -> Error "Date.make returned the wrong date fields"
  | Error _ -> Error "Date.make rejected a valid leap-year date"

let test_make_invalid_date = fun _ctx ->
  match Date.make ~year:2_025 ~month:2 ~day:29 with
  | Ok _ -> Error "Date.make should reject invalid civil dates"
  | Error (Date.Invalid_date _) -> Ok ()
  | Error _ -> Error "Date.make returned the wrong error for an invalid date"

let test_iso8601_roundtrip = fun _ctx ->
  match Date.from_iso8601 "2024-01-15" with
  | Ok date when String.equal (Date.to_iso8601 date) "2024-01-15" -> Ok ()
  | Ok _ -> Error "Date ISO roundtrip produced the wrong string"
  | Error _ -> Error "Date.from_iso8601 failed on a valid extended ISO date"

let test_add_and_diff_days = fun _ctx ->
  let start =
    Date.make ~year:2_024 ~month:1 ~day:15
    |> Result.unwrap
  in
  let finish = Date.add_days start 17 in
  if not (Int.equal (Date.diff_days finish start) 17) then
    Error "Date.diff_days should return the day delta produced by Date.add_days"
  else if not (String.equal (Date.to_iso8601 finish) "2024-02-01") then
    Error "Date.add_days crossed the month boundary incorrectly"
  else
    Ok ()

let test_of_date_time_discards_time_fields = fun _ctx ->
  let date_time =
    DateTime.parse "2025-08-27T14:07:31Z"
    |> Result.unwrap
  in
  let date = Date.from_date_time date_time in
  if date.year = 2_025 && date.month = 8 && date.day = 27 then
    Ok ()
  else
    Error "Date.from_date_time should keep only the civil date fields"

let test_today_utc_matches_datetime_now_utc = fun _ctx ->
  let date = Date.today_utc () in
  let now = DateTime.now_utc () in
  if date.year = now.year && date.month = now.month && date.day = now.day then
    Ok ()
  else
    Error "Date.today_utc should agree with DateTime.now_utc on the civil date"

let tests =
  Test.[
    case "make accepts a valid civil date" test_make_valid_date;
    case "make rejects an invalid civil date" test_make_invalid_date;
    case "ISO 8601 parsing roundtrips" test_iso8601_roundtrip;
    case "add_days and diff_days agree across month boundaries" test_add_and_diff_days;
    case "from_date_time keeps the civil date only" test_of_date_time_discards_time_fields;
    case "today_utc matches DateTime.now_utc" test_today_utc_matches_datetime_now_utc;
  ]

let main ~args = Test.Cli.main ~name:"date" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
