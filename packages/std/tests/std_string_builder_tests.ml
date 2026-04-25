open Std

let rune = fun code -> Unicode.Rune.from_int code |> Option.unwrap

let test_create_starts_empty = fun _ctx ->
  let buffer = StringBuilder.create ~size:0 in
  if Int.equal (StringBuilder.length buffer) 0 && String.equal (StringBuilder.contents buffer) "" then
    Ok ()
  else
    Error "StringBuilder.create should start empty"

let test_add_string_and_subbytes_preserve_order = fun _ctx ->
  let buffer = StringBuilder.create ~size:2 in
  StringBuilder.add_string buffer "pre";
  StringBuilder.add_subbytes buffer (IO.Bytes.from_string "abcdef") 2 3;
  if String.equal (StringBuilder.contents buffer) "precde" then
    Ok ()
  else
    Error "StringBuilder should append strings and byte slices in order"

let test_add_utf_8_uchar_materializes_utf8 = fun _ctx ->
  let buffer = StringBuilder.create ~size:2 in
  StringBuilder.add_utf_8_uchar buffer (rune 0x03b1);
  if String.equal (StringBuilder.contents buffer) "α" then
    Ok ()
  else
    Error "StringBuilder.add_utf_8_uchar should append UTF-8 text"

let tests =
  Test.[
    case "create starts empty" test_create_starts_empty;
    case "string and byte slices preserve order" test_add_string_and_subbytes_preserve_order;
    case "utf-8 runes materialize as text" test_add_utf_8_uchar_materializes_utf8;
  ]

let main ~args = Test.Cli.main ~name:"string_builder" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
