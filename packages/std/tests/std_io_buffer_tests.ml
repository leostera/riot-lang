open Std

let rune = fun code -> Unicode.Rune.from_int code |> Option.unwrap

let test_create_starts_empty = fun _ctx ->
  let buffer = IO.Buffer.create ~size:0 in
  if Int.equal (IO.Buffer.length buffer) 0 && String.equal (IO.Buffer.contents buffer) "" then
    Ok ()
  else
    Error "IO.Buffer.create should start empty"

let test_add_char_twice_preserves_order = fun _ctx ->
  let buffer = IO.Buffer.create ~size:1 in
  IO.Buffer.add_char buffer 'a';
  IO.Buffer.add_char buffer 'b';
  if String.equal (IO.Buffer.contents buffer) "ab" then
    Ok ()
  else
    Error "IO.Buffer.add_char should append chars in order"

let test_add_string_appends_entire_string = fun _ctx ->
  let buffer = IO.Buffer.create ~size:2 in
  IO.Buffer.add_string buffer "hello";
  if String.equal (IO.Buffer.contents buffer) "hello" then
    Ok ()
  else
    Error "IO.Buffer.add_string should append the full string"

let test_add_bytes_appends_entire_payload = fun _ctx ->
  let buffer = IO.Buffer.create ~size:2 in
  IO.Buffer.add_bytes buffer (IO.Bytes.from_string "abc");
  if String.equal (IO.Buffer.contents buffer) "abc" then
    Ok ()
  else
    Error "IO.Buffer.add_bytes should append the full bytes payload"

let test_add_subbytes_appends_exact_slice = fun _ctx ->
  let buffer = IO.Buffer.create ~size:2 in
  IO.Buffer.add_subbytes buffer (IO.Bytes.from_string "abcdef") 2 3;
  if String.equal (IO.Buffer.contents buffer) "cde" then
    Ok ()
  else
    Error "IO.Buffer.add_subbytes should append exactly the requested slice"

let test_add_subbytes_zero_length_is_noop = fun _ctx ->
  let buffer = IO.Buffer.create ~size:2 in
  IO.Buffer.add_string buffer "prefix";
  IO.Buffer.add_subbytes buffer (IO.Bytes.from_string "abcdef") 2 0;
  if String.equal (IO.Buffer.contents buffer) "prefix" then
    Ok ()
  else
    Error "IO.Buffer.add_subbytes should treat zero-length slices as a no-op"

let test_add_substring_appends_exact_slice = fun _ctx ->
  let buffer = IO.Buffer.create ~size:2 in
  IO.Buffer.add_substring buffer "abcdef" 1 4;
  if String.equal (IO.Buffer.contents buffer) "bcde" then
    Ok ()
  else
    Error "IO.Buffer.add_substring should append exactly the requested slice"

let test_add_substring_zero_length_is_noop = fun _ctx ->
  let buffer = IO.Buffer.create ~size:2 in
  IO.Buffer.add_string buffer "prefix";
  IO.Buffer.add_substring buffer "abcdef" 1 0;
  if String.equal (IO.Buffer.contents buffer) "prefix" then
    Ok ()
  else
    Error "IO.Buffer.add_substring should treat zero-length slices as a no-op"

let test_add_utf_8_uchar_encodes_multibyte_rune = fun _ctx ->
  let buffer = IO.Buffer.create ~size:2 in
  IO.Buffer.add_utf_8_uchar buffer (rune 0x03B1);
  if String.equal (IO.Buffer.contents buffer) "α" then
    Ok ()
  else
    Error "IO.Buffer.add_utf_8_uchar should append UTF-8 encoded runes"

let test_get_returns_some_for_valid_index_and_none_for_invalid = fun _ctx ->
  let buffer = IO.Buffer.create ~size:2 in
  IO.Buffer.add_string buffer "abc" ;
  match (IO.Buffer.get buffer ~at:1, IO.Buffer.get buffer ~at:9) with
  | Some value, None when Char.equal value 'b' -> Ok ()
  | _ -> Error "IO.Buffer.get should reflect valid and invalid indices"

let test_clear_resets_length_and_contents = fun _ctx ->
  let buffer = IO.Buffer.create ~size:2 in
  IO.Buffer.add_string buffer "abc";
  IO.Buffer.clear buffer;
  if Int.equal (IO.Buffer.length buffer) 0 && String.equal (IO.Buffer.contents buffer) "" then
    Ok ()
  else
    Error "IO.Buffer.clear should reset the buffer"

let tests = Test.[
  case "create starts empty" test_create_starts_empty;
  case "add_char preserves order" test_add_char_twice_preserves_order;
  case "add_string appends the whole string" test_add_string_appends_entire_string;
  case "add_bytes appends the whole bytes payload" test_add_bytes_appends_entire_payload;
  case "add_subbytes appends the requested slice" test_add_subbytes_appends_exact_slice;
  case "add_subbytes zero length is a no-op" test_add_subbytes_zero_length_is_noop;
  case "add_substring appends the requested slice" test_add_substring_appends_exact_slice;
  case "add_substring zero length is a no-op" test_add_substring_zero_length_is_noop;
  case "add_utf_8_uchar encodes multibyte runes" test_add_utf_8_uchar_encodes_multibyte_rune;
  case "get reflects valid and invalid indices" test_get_returns_some_for_valid_index_and_none_for_invalid;
  case "clear resets length and contents" test_clear_resets_length_and_contents;
]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name:"io_buffer" ~tests ~args) ~args:Env.args ()
