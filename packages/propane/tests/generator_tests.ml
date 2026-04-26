(** Unit tests for Generator module *)
open Std
open Propane

let make_rng = fun seed ->
  Random.Rng.standard ~seed:(Int.to_string seed) ()
  |> Result.expect ~msg:"failed to create deterministic rng"

let expect_raises = fun thunk ->
  try
    let _ = thunk () in
    Error "expected generator constructor to raise"
  with
  | _ -> Ok ()

let test_int_range_stays_within_bounds = fun _ctx ->
  let gen = Generator.int_range 5 10 in
  let rnd = make_rng 42 in
  let rec loop remaining seen_low seen_high =
    if remaining = 0 then
      if seen_low && seen_high then
        Ok ()
      else
        Error "int_range did not reach both inclusive bounds"
    else
      let value = Generator.generate rnd gen in
      if value < 5 || value > 10 then
        Error ("int_range produced out-of-range value: " ^ Int.to_string value)
      else
        loop (remaining - 1) (seen_low || value = 5) (seen_high || value = 10)
  in
  loop 500 false false

let test_one_of_empty_raises = fun _ctx -> expect_raises (fun () -> Generator.one_of [])

let test_frequency_rejects_non_positive_weights = fun _ctx ->
  match expect_raises (fun () -> Generator.frequency [ (0, Generator.return 1); ]) with
  | Error _ as err -> err
  | Ok () -> expect_raises (fun () -> Generator.frequency [ ((-1), Generator.return 1); ])

let test_sized_receives_the_ambient_size = fun _ctx ->
  let gen = Generator.sized Generator.return in
  let rnd = make_rng 7 in
  let value = Generator.generate_with_size rnd 23 gen in
  if value = 23 then
    Ok ()
  else
    Error ("expected sized generator to receive 23, got " ^ Int.to_string value)

let test_resize_overrides_the_ambient_size = fun _ctx ->
  let gen = Generator.resize 9 (Generator.sized Generator.return) in
  let rnd = make_rng 9 in
  let value = Generator.generate_with_size rnd 3 gen in
  if value = 9 then
    Ok ()
  else
    Error ("expected resize to force size 9, got " ^ Int.to_string value)

let test_char_printable_excludes_controls = fun _ctx ->
  let rnd = make_rng 11 in
  let rec loop remaining =
    if remaining = 0 then
      Ok ()
    else
      let value = Generator.generate rnd Generator.char_printable in
      if Char.code value < Char.code ' ' || Char.code value > Char.code '~' then
        Error ("char_printable produced a non-printable byte: " ^ Int.to_string (Char.code value))
      else
        loop (remaining - 1)
  in
  loop 300

let test_rune_printable_produces_printable_runes = fun _ctx ->
  let rnd = make_rng 12 in
  let rec loop remaining =
    if remaining = 0 then
      Ok ()
    else
      let value = Generator.generate rnd Generator.rune_printable in
      if Unicode.Rune.is_print value then
        loop (remaining - 1)
      else
        Error ("rune_printable produced a non-printable rune: "
        ^ Int.to_string (Unicode.Rune.to_int value))
  in
  loop 200

let test_string_size_uses_the_requested_length = fun _ctx ->
  let rnd = make_rng 13 in
  let gen = Generator.string_size (Generator.return 8) Generator.char_lowercase in
  let value = Generator.generate_with_size rnd 2 gen in
  if String.length value = 8 then
    Ok ()
  else
    Error ("expected string_size to produce length 8, got " ^ Int.to_string (String.length value))

let test_frequency_bias_is_observable = fun _ctx ->
  let rnd = make_rng 14 in
  let gen = Generator.frequency [ (9, Generator.return 0); (1, Generator.return 1); ] in
  let rec loop remaining zeroes ones =
    if remaining = 0 then
      if zeroes > ones then
        Ok ()
      else
        Error "frequency did not favor the heavier branch"
    else
      match Generator.generate rnd gen with
      | 0 -> loop (remaining - 1) (zeroes + 1) ones
      | 1 -> loop (remaining - 1) zeroes (ones + 1)
      | _ -> Error "frequency produced an unexpected value"
  in
  loop 500 0 0

let test_float_spans_both_signs = fun _ctx ->
  let rnd = make_rng 15 in
  let rec loop remaining seen_positive seen_negative =
    if remaining = 0 then
      if seen_positive && seen_negative then
        Ok ()
      else
        Error "float generator did not cover both signs"
    else
      let value = Generator.generate rnd Generator.float in
      loop (remaining - 1) (seen_positive || value > 0.0) (seen_negative || value < 0.0)
  in
  loop 200 false false

let tests =
  Test.[
    case "int_range stays within bounds and reaches both ends" test_int_range_stays_within_bounds;
    case "one_of rejects an empty input" test_one_of_empty_raises;
    case "frequency rejects non-positive weights" test_frequency_rejects_non_positive_weights;
    case "sized receives the ambient size" test_sized_receives_the_ambient_size;
    case "resize overrides the ambient size" test_resize_overrides_the_ambient_size;
    case "char_printable excludes control bytes" test_char_printable_excludes_controls;
    case "rune_printable produces printable runes" test_rune_printable_produces_printable_runes;
    case "string_size uses the requested length" test_string_size_uses_the_requested_length;
    case "frequency bias is observable" test_frequency_bias_is_observable;
    case "float spans both signs" test_float_spans_both_signs;
  ]

let main ~args = Test.Cli.main ~name:"propane/generator_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
