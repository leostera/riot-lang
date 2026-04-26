open Std

module Test = Std.Test
module Utf8_reader = Tty.Utf8_reader

type chunk =
  | Data of string
  | Would_block
  | Error

let make_read = fun chunks ->
  let remaining = ref chunks in
  fun bytes ~offset ~len ->
    match !remaining with
    | [] -> `Ok 0
    | Would_block :: rest ->
        remaining := rest;
        `Would_block
    | Error :: rest ->
        remaining := rest;
        `Error
    | (Data chunk) :: rest ->
        let count = Int.min len (String.length chunk) in
        IO.Bytes.blit_string chunk ~src_offset:0 ~dst:bytes ~dst_offset:offset ~len:count;
        if Int.equal count (String.length chunk) then
          remaining := rest
        else
          remaining := Data (String.sub chunk ~offset:count ~len:(String.length chunk - count))
          :: rest;
        `Ok count

let test_ascii = fun _ctx ->
  let reader = Utf8_reader.create () in
  match Utf8_reader.read reader ~read:(make_read [ Data "a" ]) with
  | `Read "a" -> Ok ()
  | _ -> Error "Expected ASCII input to read as one rune"

let test_two_byte_rune = fun _ctx ->
  let reader = Utf8_reader.create () in
  match Utf8_reader.read reader ~read:(make_read [ Data "é" ]) with
  | `Read "é" -> Ok ()
  | _ -> Error "Expected two-byte rune to read successfully"

let test_three_byte_rune = fun _ctx ->
  let reader = Utf8_reader.create () in
  match Utf8_reader.read reader ~read:(make_read [ Data "€" ]) with
  | `Read "€" -> Ok ()
  | _ -> Error "Expected three-byte rune to read successfully"

let test_four_byte_rune = fun _ctx ->
  let reader = Utf8_reader.create () in
  match Utf8_reader.read reader ~read:(make_read [ Data "🙂" ]) with
  | `Read "🙂" -> Ok ()
  | _ -> Error "Expected four-byte rune to read successfully"

let test_invalid_start_byte = fun _ctx ->
  let reader = Utf8_reader.create () in
  match Utf8_reader.read reader ~read:(make_read [ Data "\xff" ]) with
  | `Malformed "Invalid UTF-8 start byte" -> Ok ()
  | _ -> Error "Expected invalid start byte to be rejected"

let test_invalid_continuation_byte = fun _ctx ->
  let reader = Utf8_reader.create () in
  match Utf8_reader.read reader ~read:(make_read [ Data "\xc3"; Data "x" ]) with
  | `Malformed "Invalid UTF-8 sequence" -> Ok ()
  | _ -> Error "Expected invalid continuation byte to be rejected"

let test_partial_sequence_retries_and_recovers = fun _ctx ->
  let reader = Utf8_reader.create () in
  let read = make_read [ Data "\xc3"; Would_block; Data "\xa9" ] in
  match Utf8_reader.read reader ~read with
  | `Retry -> (
      match Utf8_reader.read reader ~read with
      | `Read "é" -> Ok ()
      | _ -> Error "Expected a pending two-byte rune to resume after retry"
    )
  | _ -> Error "Expected a partial multi-byte read to yield Retry"

let test_incomplete_sequence_at_end = fun _ctx ->
  let reader = Utf8_reader.create () in
  let read = make_read [ Data "\xe2"; Data "\x82" ] in
  match Utf8_reader.read reader ~read with
  | `Retry -> (
      match Utf8_reader.read reader ~read with
      | `Malformed "Incomplete UTF-8 sequence" -> Ok ()
      | _ -> Error "Expected EOF during a partial sequence to report incomplete UTF-8"
    )
  | _ -> Error "Expected partial three-byte rune to remain pending"

let test_end_of_stream = fun _ctx ->
  let reader = Utf8_reader.create () in
  match Utf8_reader.read reader ~read:(make_read []) with
  | `End -> Ok ()
  | _ -> Error "Expected empty input to report end of stream"

let tests =
  Test.[
    case "ascii" test_ascii;
    case "two_byte_rune" test_two_byte_rune;
    case "three_byte_rune" test_three_byte_rune;
    case "four_byte_rune" test_four_byte_rune;
    case "invalid_start_byte" test_invalid_start_byte;
    case "invalid_continuation_byte" test_invalid_continuation_byte;
    case "partial_sequence_retries_and_recovers" test_partial_sequence_retries_and_recovers;
    case "incomplete_sequence_at_end" test_incomplete_sequence_at_end;
    case "end_of_stream" test_end_of_stream;
  ]

let main ~args = Test.Cli.main ~name:"tty_utf8_reader" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
