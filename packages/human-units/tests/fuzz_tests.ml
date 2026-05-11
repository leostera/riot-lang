open Std

module Test = Std.Test
module Units = Human_units

let byte_dictionary = [
  "";
  "0";
  "1";
  "550";
  "550 KiB";
  "550MiB";
  "1.5 KiB";
  "2.5 TiB";
  "999999999999999999999999999999999999999999999999999 EiB";
  "B";
  "KB";
  "KiB";
  "MiB";
  "GiB";
  "TiB";
  "PiB";
  "EiB";
  "wat";
]

let duration_dictionary = [
  "";
  "0";
  "1ns";
  "12us";
  "12µs";
  "1.5ms";
  "4.2s";
  "6mins 2secs";
  "2years 2mins 12us";
  "1.234s0.345ms0.678us0ns";
  "999999999999999999999999999999999999999999years";
  "ns";
  "us";
  "µs";
  "ms";
  "secs";
  "mins";
  "hrs";
  "days";
  "weeks";
  "months";
  "years";
  "wat";
]

let byte_count_mutator =
  Test.Fuzz.Mutator.(text
  |> with_max_len 256
  |> with_dictionary ("4611686018427387903" :: byte_dictionary))

let byte_text_mutator =
  Test.Fuzz.Mutator.(text
  |> with_max_len 512
  |> with_dictionary byte_dictionary)

let duration_count_mutator =
  Test.Fuzz.Mutator.(text
  |> with_max_len 256
  |> with_dictionary ("4611686018427387903" :: duration_dictionary))

let duration_text_mutator =
  Test.Fuzz.Mutator.(text
  |> with_max_len 512
  |> with_dictionary duration_dictionary)

let bounded_hash = fun ~limit input ->
  let acc = ref 0 in
  for index = 0 to String.length input - 1 do
    let byte = Char.code (String.get_unchecked input ~at:index) in
    acc := ((!acc * 131) + byte) mod limit
  done;
  !acc

let input_to_nonnegative_int = fun ~fallback_limit input ->
  match Int.parse (String.trim input) with
  | Some value when value >= 0 -> value
  | Some _
  | None -> bounded_hash ~limit:fallback_limit input

let accept_bytes_parse = fun result ->
  match result with
  | Ok count ->
      Units.bytes count
      |> ignore;
      Ok ()
  | Error _ -> Ok ()

let accept_duration_parse = fun result ->
  match result with
  | Ok duration ->
      Units.duration duration
      |> ignore;
      Ok ()
  | Error _ -> Ok ()

let test_bytes_formatter_fuzz = fun _ctx input ->
  let count = input_to_nonnegative_int ~fallback_limit:10_995_116_277_760 input in
  Units.bytes count
  |> ignore;
  Ok ()

let test_parse_bytes_fuzz = fun _ctx input ->
  Units.parse_bytes input
  |> accept_bytes_parse

let test_duration_formatter_fuzz = fun _ctx input ->
  let nanos = input_to_nonnegative_int ~fallback_limit:31_557_600_000_000_000 input in
  Units.duration (Time.Duration.from_nanos nanos)
  |> ignore;
  Ok ()

let test_parse_duration_fuzz = fun _ctx input ->
  Units.parse_duration input
  |> accept_duration_parse

let tests =
  Test.[
    fuzz
      "bytes formatter accepts arbitrary derived counts"
      ~seeds:[ ""; "0"; "1"; "563200"; "4611686018427387903"; ]
      ~mutator:byte_count_mutator
      test_bytes_formatter_fuzz;
    fuzz
      "parse_bytes accepts arbitrary text"
      ~seeds:[
        "";
        "0";
        "550 KiB";
        "550MiB";
        "1.5 KiB";
        "999999999999999999999999999999999999999999999999999 EiB";
      ]
      ~mutator:byte_text_mutator
      test_parse_bytes_fuzz;
    fuzz
      "duration formatter accepts arbitrary derived nanos"
      ~seeds:[ ""; "0"; "12202"; "362000000000"; "4611686018427387903"; ]
      ~mutator:duration_count_mutator
      test_duration_formatter_fuzz;
    fuzz
      "parse_duration accepts arbitrary text"
      ~seeds:[
        "";
        "0";
        "2years 2mins 12us";
        "1.234s0.345ms0.678us0ns";
        "0.5ns";
        "999999999999999999999999999999999999999999years";
      ]
      ~mutator:duration_text_mutator
      test_parse_duration_fuzz;
  ]

let main ~args = Test.Cli.main ~name:"human_units_fuzz_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
