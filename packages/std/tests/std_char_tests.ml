open Std

let test_from_int_65 = fun _ctx ->
  match Char.from_int 65 with
  | Some 'A' -> Ok ()
  | _ -> Error "expected Char.from_int 65 = Some 'A'"

let test_from_int_0 = fun _ctx ->
  match Char.from_int 0 with
  | Some c when Char.to_int c = 0 -> Ok ()
  | _ -> Error "expected Char.from_int 0 = Some '\\x00'"

let test_from_int_255 = fun _ctx ->
  match Char.from_int 255 with
  | Some c when Char.to_int c = 255 -> Ok ()
  | _ -> Error "expected Char.from_int 255 to succeed"

let test_from_int_negative = fun _ctx ->
  match Char.from_int (-1) with
  | None -> Ok ()
  | Some _ -> Error "expected Char.from_int -1 = None"

let test_from_int_too_large = fun _ctx ->
  match Char.from_int 256 with
  | None -> Ok ()
  | Some _ -> Error "expected Char.from_int 256 = None"

let test_from_int_unchecked = fun _ctx ->
  if Char.equal (Char.from_int_unchecked 97) 'a' then
    Ok ()
  else
    Error "expected unchecked 97 = 'a'"

let test_to_int = fun _ctx ->
  if Int.equal (Char.to_int 'A') 65 then
    Ok ()
  else
    Error "expected Char.to_int 'A' = 65"

let test_code = fun _ctx ->
  if Int.equal (Char.code '\n') 10 then
    Ok ()
  else
    Error "expected Char.code newline = 10"

let test_lowercase_ascii = fun _ctx ->
  if Char.equal (Char.lowercase_ascii 'A') 'a' then
    Ok ()
  else
    Error "expected lowercase_ascii 'A' = 'a'"

let test_lowercase_ascii_non_alpha = fun _ctx ->
  if Char.equal (Char.lowercase_ascii '!') '!' then
    Ok ()
  else
    Error "expected lowercase_ascii ! unchanged"

let test_uppercase_ascii = fun _ctx ->
  if Char.equal (Char.uppercase_ascii 'z') 'Z' then
    Ok ()
  else
    Error "expected uppercase_ascii 'z' = 'Z'"

let test_uppercase_ascii_non_alpha = fun _ctx ->
  if Char.equal (Char.uppercase_ascii '5') '5' then
    Ok ()
  else
    Error "expected uppercase_ascii 5 unchanged"

let tests =
  Test.[
    case "Char.from_int 65" test_from_int_65;
    case "Char.from_int 0" test_from_int_0;
    case "Char.from_int 255" test_from_int_255;
    case "Char.from_int -1" test_from_int_negative;
    case "Char.from_int 256" test_from_int_too_large;
    case "Char.from_int_unchecked 97" test_from_int_unchecked;
    case "Char.to_int 'A'" test_to_int;
    case "Char.code newline" test_code;
    case "Char.lowercase_ascii uppercases only letters" test_lowercase_ascii;
    case "Char.lowercase_ascii leaves punctuation unchanged" test_lowercase_ascii_non_alpha;
    case "Char.uppercase_ascii lowercases only letters" test_uppercase_ascii;
    case "Char.uppercase_ascii leaves digits unchanged" test_uppercase_ascii_non_alpha;
  ]

let main ~args = Test.Cli.main ~name:"char" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
