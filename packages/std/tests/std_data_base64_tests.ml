open Std
module Base64 = Std.Data.Base64

let test_encode_simple () = Error "todo"
let test_encode_empty () = Error "todo"
let test_encode_bytes () = Error "todo"
let test_decode_simple () = Error "todo"
let test_decode_invalid_char () = Error "todo"
let test_roundtrip () = Error "todo"
let test_roundtrip_binary () = Error "todo"
let test_padding () = Error "todo"

let tests =
  Test.
    [
      case "encode simple" test_encode_simple;
      case "encode empty" test_encode_empty;
      case "encode bytes" test_encode_bytes;
      case "decode simple" test_decode_simple;
      case "decode invalid char" test_decode_invalid_char;
      case "roundtrip" test_roundtrip;
      case "binary roundtrip" test_roundtrip_binary;
      case "padding" test_padding;
    ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"base64" ~tests ~args)
    ~args:Env.args ()
