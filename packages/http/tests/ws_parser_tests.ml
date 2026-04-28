open Std

module Frame = Http.Ws.Frame
module Parser = Http.Ws.Parser

let expect_parse_error = fun ~role bytes expected ->
  match Parser.parse ~role bytes with
  | Parser.Error err when err = expected -> Result.Ok ()
  | Parser.Error err -> Result.Error ("Wrong parse error: " ^ Parser.error_to_string err)
  | Parser.Need_more -> Result.Error "Expected parse error"
  | Parser.Done _ -> Result.Error "Expected parse error, but frame parsed"

let test_parse_valid_ping = fun _ctx ->
  match Parser.parse ~role:Parser.Client "\x89\x00" with
  | Parser.Done { value = { Frame.opcode = Frame.Ping; fin = true; payload = ""; _ }; remaining = "" } ->
      Result.Ok ()
  | Parser.Done _ -> Result.Error "PING frame parsed with the wrong shape"
  | Parser.Need_more -> Result.Error "PING frame unexpectedly needed more data"
  | Parser.Error err -> Result.Error ("PING frame was rejected: " ^ Parser.error_to_string err)

let test_parse_valid_masked_client_ping = fun _ctx ->
  match Parser.parse ~role:Parser.Server "\x89\x80\x00\x00\x00\x00" with
  | Parser.Done { value = {
    Frame.opcode = Frame.Ping;
    fin = true;
    masked = true;
    payload = "";
    _
  }; remaining = "" } -> Result.Ok ()
  | Parser.Done _ -> Result.Error "masked PING frame parsed with the wrong shape"
  | Parser.Need_more -> Result.Error "masked PING frame unexpectedly needed more data"
  | Parser.Error err ->
      Result.Error ("masked PING frame was rejected: " ^ Parser.error_to_string err)

let test_parse_rejects_unmasked_client_frame = fun _ctx ->
  expect_parse_error
    ~role:Parser.Server
    "\x89\x00"
    Parser.ClientFrameNotMasked

let test_parse_rejects_masked_server_frame = fun _ctx ->
  expect_parse_error
    ~role:Parser.Client
    "\x89\x80\x00\x00\x00\x00"
    Parser.ServerFrameMasked

let test_parse_rejects_rsv_bits = fun _ctx ->
  expect_parse_error
    ~role:Parser.Client
    "\xc1\x00"
    Parser.ReservedBitsSet

let test_parse_rejects_fragmented_ping = fun _ctx ->
  expect_parse_error
    ~role:Parser.Client
    "\x09\x00"
    Parser.FragmentedControlFrame

let test_parse_rejects_oversized_control_length = fun _ctx ->
  expect_parse_error
    ~role:Parser.Client
    "\x89\x7e\x00\x7e"
    (Parser.ControlFramePayloadTooLarge { payload_length = 126 })

let test_parse_64_bit_length_uses_high_bytes = fun _ctx ->
  match Parser.parse ~role:Parser.Client "\x82\x7f\x00\x00\x00\x01\x00\x00\x00\x00" with
  | Parser.Need_more -> Result.Ok ()
  | Parser.Error err -> Result.Error ("64-bit length was rejected: " ^ Parser.error_to_string err)
  | Parser.Done _ -> Result.Error "64-bit length was truncated to the low 32 bits"

let test_parse_rejects_64_bit_length_high_bit = fun _ctx ->
  expect_parse_error
    ~role:Parser.Client
    "\x82\x7f\x80\x00\x00\x00\x00\x00\x00\x00"
    (Parser.PayloadLengthHighBitSet { first_byte = 0x80 })

let test_parse_rejects_64_bit_length_above_int_max = fun _ctx ->
  expect_parse_error
    ~role:Parser.Client
    "\x82\x7f\x40\x00\x00\x00\x00\x00\x00\x00"
    (Parser.PayloadLengthTooLarge { most_significant_byte = 0x40; max_payload_length = Int.max_int })

let test_parse_rejects_payload_over_limit = fun _ctx ->
  match Parser.parse ~max_payload_length:2 ~role:Parser.Client "\x81\x03abc" with
  | Parser.Error (Parser.PayloadLengthExceedsLimit { payload_length = 3; max_payload_length = 2 }) ->
      Result.Ok ()
  | Parser.Error err -> Result.Error ("Wrong parse error: " ^ Parser.error_to_string err)
  | Parser.Need_more -> Result.Error "Expected payload limit error"
  | Parser.Done _ -> Result.Error "Frame over payload limit was accepted"

let test_parse_rejects_negative_payload_limit = fun _ctx ->
  match Parser.parse ~max_payload_length:(-1) ~role:Parser.Client "\x81\x00" with
  | Parser.Error (Parser.InvalidPayloadLengthLimit { max_payload_length }) when Int.equal
    max_payload_length
    (-1) -> Result.Ok ()
  | Parser.Error err -> Result.Error ("Wrong parse error: " ^ Parser.error_to_string err)
  | Parser.Need_more -> Result.Error "Expected invalid payload limit error"
  | Parser.Done _ -> Result.Error "Negative payload limit was accepted"

let tests =
  Test.[
    case "parse_valid_ping" test_parse_valid_ping;
    case "parse_valid_masked_client_ping" test_parse_valid_masked_client_ping;
    case "parse_rejects_unmasked_client_frame" test_parse_rejects_unmasked_client_frame;
    case "parse_rejects_masked_server_frame" test_parse_rejects_masked_server_frame;
    case "parse_rejects_rsv_bits" test_parse_rejects_rsv_bits;
    case "parse_rejects_fragmented_ping" test_parse_rejects_fragmented_ping;
    case "parse_rejects_oversized_control_length" test_parse_rejects_oversized_control_length;
    case "parse_64_bit_length_uses_high_bytes" test_parse_64_bit_length_uses_high_bytes;
    case "parse_rejects_64_bit_length_high_bit" test_parse_rejects_64_bit_length_high_bit;
    case "parse_rejects_64_bit_length_above_int_max" test_parse_rejects_64_bit_length_above_int_max;
    case "parse_rejects_payload_over_limit" test_parse_rejects_payload_over_limit;
    case "parse_rejects_negative_payload_limit" test_parse_rejects_negative_payload_limit;
  ]

let main ~args:_ = Test.Cli.main ~name:"http:ws_parser" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
