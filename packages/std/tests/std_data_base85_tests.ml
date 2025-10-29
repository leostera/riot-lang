open Std
module Base85 = Std.Data.Base85

let test_encode_simple () = Error "todo"
let test_encode_empty () = Error "todo"
let test_encode_bytes () = Error "todo"
let test_encode_zeros () = Error "todo"
let test_decode_simple () = Error "todo"
let test_decode_zeros () = Error "todo"
let test_decode_invalid_char () = Error "todo"
let test_roundtrip () = Error "todo"
let test_roundtrip_binary () = Error "todo"

let tests =
  Test.
    [
      case "encode simple" test_encode_simple;
      case "encode empty" test_encode_empty;
      case "encode bytes" test_encode_bytes;
      case "encode zeros" test_encode_zeros;
      case "decode simple" test_decode_simple;
      case "decode zeros" test_decode_zeros;
      case "decode invalid char" test_decode_invalid_char;
      case "roundtrip" test_roundtrip;
      case "binary roundtrip" test_roundtrip_binary;
    ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"base85" ~tests ~args)
    ~args:Env.args ()
