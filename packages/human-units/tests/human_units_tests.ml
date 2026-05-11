open Std

module Units = Human_units

let ( let* ) result fn = Result.and_then result ~fn

let expect_string = fun ~expected ~actual ->
  if String.equal expected actual then
    Ok ()
  else
    Error ("expected " ^ expected ^ ", got " ^ actual)

let expect_int = fun ~expected ~actual ->
  if Int.equal expected actual then
    Ok ()
  else
    Error ("expected " ^ Int.to_string expected ^ ", got " ^ Int.to_string actual)

let expect_duration = fun ~expected_nanos ~actual ->
  let actual_nanos =
    Time.Duration.to_nanos actual
    |> Int64.to_int
  in
  expect_int ~expected:expected_nanos ~actual:actual_nanos

let expect_parse_bytes = fun ~expected input ->
  match Units.parse_bytes input with
  | Ok actual -> expect_int ~expected ~actual
  | Error error -> Error (Units.error_to_string error)

let expect_parse_duration = fun ~expected_nanos input ->
  match Units.parse_duration input with
  | Ok actual -> expect_duration ~expected_nanos ~actual
  | Error error -> Error (Units.error_to_string error)

let expect_parse_error = fun result ->
  match result with
  | Ok _ -> Error "expected parse error"
  | Error _ -> Ok ()

let test_bytes_formats_binary_units = fun _ctx ->
  let* () = expect_string ~expected:"0 B" ~actual:(Units.bytes 0) in
  let* () = expect_string ~expected:"550 B" ~actual:(Units.bytes 550) in
  let* () = expect_string ~expected:"550 KiB" ~actual:(Units.bytes 563_200) in
  let* () = expect_string ~expected:"650 MiB" ~actual:(Units.bytes 681_574_400) in
  let* () = expect_string ~expected:"15.3 GiB" ~actual:(Units.bytes 16_428_249_907) in
  let tebibyte = 1_099_511_627_776 in
  expect_string ~expected:"2.5 TiB" ~actual:(Units.bytes ((tebibyte * 2) + (tebibyte / 2)))

let test_parse_bytes_accepts_binary_and_decimal_units = fun _ctx ->
  let* () = expect_parse_bytes ~expected:550 "550" in
  let* () = expect_parse_bytes ~expected:563_200 "550 KiB" in
  let* () = expect_parse_bytes ~expected:563_200 "550KiB" in
  let* () = expect_parse_bytes ~expected:576_716_800 "550 MiB" in
  let* () = expect_parse_bytes ~expected:550_000 "550 KB" in
  let* () = expect_parse_bytes ~expected:1_536 "1.5 KiB" in
  expect_parse_bytes ~expected:2_748_779_069_440 "2.5 TiB"

let test_parse_bytes_rejects_invalid_inputs = fun _ctx ->
  let* () = expect_parse_error (Units.parse_bytes "") in
  let* () = expect_parse_error (Units.parse_bytes "wat") in
  expect_parse_error (Units.parse_bytes "10 XB")

let test_parse_duration_accepts_human_readable_units = fun _ctx ->
  let year = 31_557_600 * 1_000_000_000 in
  let* () = expect_parse_duration ~expected_nanos:17 "17nsec" in
  let* () = expect_parse_duration ~expected_nanos:78_000 "78us" in
  let* () = expect_parse_duration ~expected_nanos:163_000 "163µs" in
  let* () = expect_parse_duration ~expected_nanos:31_000_000 "31msec" in
  let* () = expect_parse_duration ~expected_nanos:4_200_000_000 "4.2s" in
  let* () = expect_parse_duration ~expected_nanos:(2 * 3_600_000_000_000) "2h" in
  let* () = expect_parse_duration ~expected_nanos:(7 * 60_000_000_000) "7minutes" in
  let* () = expect_parse_duration ~expected_nanos:(12 * 2_630_016_000_000_000) "12M" in
  let* () =
    expect_parse_duration ~expected_nanos:(2 * year + 120_000_000_000 + 12_000) "2years 2mins 12us"
  in
  let* () = expect_parse_duration ~expected_nanos:7_123 "7.120us 3ns" in
  expect_parse_duration ~expected_nanos:1_234_345_678 "1.234s0.345ms0.678us0ns"

let test_parse_duration_rejects_missing_or_lossy_units = fun _ctx ->
  let* () = expect_parse_error (Units.parse_duration "") in
  let* () = expect_parse_error (Units.parse_duration "123") in
  let* () = expect_parse_error (Units.parse_duration "1nights") in
  expect_parse_error (Units.parse_duration "0.5ns")

let test_duration_formats_compact_units = fun _ctx ->
  let* () =
    expect_string
      ~expected:"6mins 2secs"
      ~actual:(Units.duration (Time.Duration.from_secs ((6 * 60) + 2)))
  in
  let* () =
    expect_string ~expected:"12.2µs" ~actual:(Units.duration (Time.Duration.from_nanos 12_202))
  in
  let* () =
    expect_string
      ~expected:"1year 2months 3days 4hrs 5mins 6secs"
      ~actual:(Units.duration
        (Time.Duration.from_secs (31_557_600 + (2 * 2_630_016) + (3 * 86_400) + (4 * 3_600) + (5
        * 60) + 6)))
  in
  expect_string
    ~expected:"1sec 1.5ms"
    ~actual:(Units.duration (Time.Duration.from_nanos 1_001_500_000))

let tests =
  Test.[
    case "bytes formats binary units" test_bytes_formats_binary_units;
    case
      "parse_bytes accepts binary and decimal units"
      test_parse_bytes_accepts_binary_and_decimal_units;
    case "parse_bytes rejects invalid inputs" test_parse_bytes_rejects_invalid_inputs;
    case
      "parse_duration accepts human-readable units"
      test_parse_duration_accepts_human_readable_units;
    case
      "parse_duration rejects missing or lossy units"
      test_parse_duration_rejects_missing_or_lossy_units;
    case "duration formats compact units" test_duration_formats_compact_units;
  ]

let main ~args = Test.Cli.main ~name:"human-units" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
