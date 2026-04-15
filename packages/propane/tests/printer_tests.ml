open Std
open Propane

let test_char_escapes_quotes_and_backslashes = fun _ctx ->
  if Printer.char '\'' = "'\\''" && Printer.char '\\' = "'\\\\'" then
    Ok ()
  else
    Error "char printer should escape quotes and backslashes"

let test_char_escapes_control_characters = fun _ctx ->
  if Printer.char '\n' = "'\\n'" && Printer.char '\t' = "'\\t'" && Printer.char '\001' = "'\\x01'" then
    Ok ()
  else
    Error "char printer should escape control characters"

let test_string_escapes_quotes_backslashes_and_controls = fun _ctx ->
  let value = "\"\\\n\t\r" in
  let printed = Printer.string value in
  if printed = "\"\\\"\\\\\\n\\t\\r\"" then
    Ok ()
  else
    Error ("string printer produced an unexpected escaped form: " ^ printed)

let test_list_and_array_formatting = fun _ctx ->
  let list_printed = Printer.list Printer.int [ 1; 2; 3 ] in
  let array_printed = Printer.array Printer.int (Collections.Array.from_list [ 1; 2 ]) in
  if list_printed = "[1; 2; 3]" && array_printed = "[|1; 2|]" then
    Ok ()
  else
    Error "list or array printer used an unexpected delimiter layout"

let test_pair_and_triple_formatting = fun _ctx ->
  let pair_printed = Printer.pair Printer.int Printer.string (1, "x") in
  let triple_printed = Printer.triple Printer.int Printer.bool Printer.string (1, true, "x") in
  if pair_printed = "(1, \"x\")" && triple_printed = "(1, true, \"x\")" then
    Ok ()
  else
    Error "tuple printers used an unexpected layout"

let test_option_and_result_formatting = fun _ctx ->
  let option_printed = Printer.option Printer.int (Some 1) in
  let result_printed = Printer.result Printer.int Printer.string (Error "boom") in
  if option_printed = "Some (1)" && result_printed = "Error (\"boom\")" then
    Ok ()
  else
    Error "option or result printer used an unexpected layout"

let test_hashmap_printer_is_stable = fun _ctx ->
  let value = Collections.HashMap.from_list [ (2, "b"); (1, "a") ] in
  let printed = Printer.hashmap Printer.int Printer.string value in
  if printed = "map{1 => \"a\"; 2 => \"b\"}" then
    Ok ()
  else
    Error ("hashmap printer should sort entries for stable reports, got: " ^ printed)

let test_hashset_printer_is_stable = fun _ctx ->
  let value = Collections.HashSet.from_list [ 3; 1; 2 ] in
  let printed = Printer.hashset Printer.int value in
  if printed = "set{1; 2; 3}" then
    Ok ()
  else
    Error ("hashset printer should sort elements for stable reports, got: " ^ printed)

let tests =
  Test.[
    case "char escapes quotes and backslashes" test_char_escapes_quotes_and_backslashes;
    case "char escapes control characters" test_char_escapes_control_characters;
    case "string escapes quotes backslashes and controls" test_string_escapes_quotes_backslashes_and_controls;
    case "list and array formatting" test_list_and_array_formatting;
    case "pair and triple formatting" test_pair_and_triple_formatting;
    case "option and result formatting" test_option_and_result_formatting;
    case "hashmap printer is stable" test_hashmap_printer_is_stable;
    case "hashset printer is stable" test_hashset_printer_is_stable;
  ]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"propane/printer_tests" ~tests ~args)
    ~args:Env.args
    ()
