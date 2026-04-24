open Std
module Bytes = IO.Bytes
module F = Std.Format

let test_format_empty = fun _ctx ->
  if String.equal (F.format []) "" then
    Ok ()
  else
    Error "Format.format [] should return the empty string"

let test_format_concatenates_fragments_in_order = fun _ctx ->
  let actual = F.format
    [
      F.str "ok:";
      F.char ' ';
      F.bool true;
      F.char ' ';
      F.int 42;
      F.char ' ';
      F.bytes (Bytes.from_string "done");
    ] in
  if String.equal actual "ok: true 42 done" then
    Ok ()
  else
    Error "Format.format should concatenate fragments in order"

let test_to_string_handles_string = fun _ctx ->
  if String.equal (F.to_string (F.str "hello")) "hello" then
    Ok ()
  else
    Error "Format.to_string should unwrap string fragments"

let test_to_string_handles_char = fun _ctx ->
  if String.equal (F.to_string (F.char 'x')) "x" then
    Ok ()
  else
    Error "Format.to_string should render chars"

let test_to_string_handles_bool = fun _ctx ->
  if String.equal (F.to_string (F.bool false)) "false" then
    Ok ()
  else
    Error "Format.to_string should render bools"

let test_to_string_handles_int = fun _ctx ->
  if String.equal (F.to_string (F.int 7)) "7" then
    Ok ()
  else
    Error "Format.to_string should render ints"

let test_to_string_handles_bytes = fun _ctx ->
  if String.equal (F.to_string (F.bytes (Bytes.from_string "raw"))) "raw" then
    Ok ()
  else
    Error "Format.to_string should render bytes"

let tests =
  Test.[
    case "format [] returns the empty string" test_format_empty;
    case "format concatenates fragments in order" test_format_concatenates_fragments_in_order;
    case "to_string renders strings" test_to_string_handles_string;
    case "to_string renders chars" test_to_string_handles_char;
    case "to_string renders bools" test_to_string_handles_bool;
    case "to_string renders ints" test_to_string_handles_int;
    case "to_string renders bytes" test_to_string_handles_bytes;
  ]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name:"format" ~tests ~args ()) ~args:Env.args ()
