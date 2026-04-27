open Std

module Frame = Http.Ws.Frame
module Parser = Http.Ws.Parser

let expect_parse_error = fun bytes expected ->
  match Parser.parse bytes with
  | Parser.Error err when err = expected -> Result.Ok ()
  | Parser.Error err -> Result.Error ("Wrong parse error: " ^ Parser.error_to_string err)
  | Parser.Need_more -> Result.Error "Expected parse error"
  | Parser.Done _ -> Result.Error "Expected parse error, but frame parsed"

let test_parse_valid_ping = fun _ctx ->
  match Parser.parse "\x89\x00" with
  | Parser.Done { value = { Frame.opcode = Frame.Ping; fin = true; payload = ""; _ }; remaining = "" } ->
      Result.Ok ()
  | Parser.Done _ -> Result.Error "PING frame parsed with the wrong shape"
  | Parser.Need_more -> Result.Error "PING frame unexpectedly needed more data"
  | Parser.Error err -> Result.Error ("PING frame was rejected: " ^ Parser.error_to_string err)

let test_parse_rejects_rsv_bits = fun _ctx -> expect_parse_error "\xc1\x00" Parser.ReservedBitsSet

let test_parse_rejects_fragmented_ping = fun _ctx ->
  expect_parse_error
    "\x09\x00"
    Parser.FragmentedControlFrame

let test_parse_rejects_oversized_control_length = fun _ctx ->
  expect_parse_error
    "\x89\x7e\x00\x7e"
    (Parser.ControlFramePayloadTooLarge { payload_length = 126 })

let tests =
  Test.[
    case "parse_valid_ping" test_parse_valid_ping;
    case "parse_rejects_rsv_bits" test_parse_rejects_rsv_bits;
    case "parse_rejects_fragmented_ping" test_parse_rejects_fragmented_ping;
    case "parse_rejects_oversized_control_length" test_parse_rejects_oversized_control_length;
  ]

let main ~args:_ = Test.Cli.main ~name:"http:ws_parser" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
