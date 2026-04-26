open Std

let position = fun line character -> Unicode.Utf16.{ line; character }

let expect_position = fun ~text ~offset ~line ~character ->
  let actual = Unicode.Utf16.position_of_offset text ~offset in
  if actual.line = line && actual.character = character then
    Ok ()
  else
    Error ("expected position "
    ^ Int.to_string line
    ^ ":"
    ^ Int.to_string character
    ^ ", got "
    ^ Int.to_string actual.line
    ^ ":"
    ^ Int.to_string actual.character)

let expect_offset = fun ~text ~position:target ~offset ->
  match Unicode.Utf16.offset_of_position text target with
  | Ok actual when actual = offset -> Ok ()
  | Ok actual -> Error ("expected offset " ^ Int.to_string offset ^ ", got " ^ Int.to_string actual)
  | Error message -> Error ("expected offset " ^ Int.to_string offset ^ ", got error: " ^ message)

let test_code_units_ascii = fun _ctx ->
  match Unicode.Utf8.decode_rune "a" 0 with
  | Some (rune, _) ->
      if Unicode.Utf16.code_units_of_rune rune = 1 then
        Ok ()
      else
        Error "ASCII rune should occupy one UTF-16 code unit"
  | None -> Error "failed to decode ASCII rune"

let test_code_units_emoji = fun _ctx ->
  match Unicode.Utf8.decode_rune "😀" 0 with
  | Some (rune, _) ->
      if Unicode.Utf16.code_units_of_rune rune = 2 then
        Ok ()
      else
        Error "supplementary-plane rune should occupy two UTF-16 code units"
  | None -> Error "failed to decode emoji rune"

let test_position_of_offset_counts_surrogate_pairs = fun _ctx ->
  expect_position ~text:"a😀b" ~offset:5 ~line:0 ~character:3

let test_offset_of_position_counts_surrogate_pairs = fun _ctx ->
  expect_offset
    ~text:"a😀b"
    ~position:(position 0 3)
    ~offset:5

let test_position_of_offset_handles_crlf = fun _ctx ->
  expect_position ~text:"a\r\n😀\nZ" ~offset:7 ~line:1 ~character:2

let test_offset_of_position_handles_crlf = fun _ctx ->
  expect_offset
    ~text:"a\r\n😀\nZ"
    ~position:(position 1 2)
    ~offset:7

let test_offset_of_position_rejects_split_surrogate_pair = fun _ctx ->
  match Unicode.Utf16.offset_of_position "😀" (position 0 1) with
  | Ok offset -> Error ("expected surrogate pair split error, got offset " ^ Int.to_string offset)
  | Error "position splits a UTF-16 surrogate pair" -> Ok ()
  | Error message -> Error ("unexpected error: " ^ message)

let test_offset_of_position_rejects_character_beyond_line = fun _ctx ->
  match Unicode.Utf16.offset_of_position "abc\nz" (position 0 5) with
  | Ok offset -> Error ("expected end-of-line error, got offset " ^ Int.to_string offset)
  | Error message when String.contains message "beyond the end of line" -> Ok ()
  | Error message -> Error ("unexpected error: " ^ message)

let tests =
  Test.[
    case "utf16 code units ascii" test_code_units_ascii;
    case "utf16 code units emoji" test_code_units_emoji;
    case
      "utf16 position_of_offset counts surrogate pairs"
      test_position_of_offset_counts_surrogate_pairs;
    case
      "utf16 offset_of_position counts surrogate pairs"
      test_offset_of_position_counts_surrogate_pairs;
    case "utf16 position_of_offset handles crlf" test_position_of_offset_handles_crlf;
    case "utf16 offset_of_position handles crlf" test_offset_of_position_handles_crlf;
    case
      "utf16 offset_of_position rejects split surrogate pair"
      test_offset_of_position_rejects_split_surrogate_pair;
    case
      "utf16 offset_of_position rejects character beyond line"
      test_offset_of_position_rejects_character_beyond_line;
  ]

let main ~args = Test.Cli.main ~name:"utf16" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
