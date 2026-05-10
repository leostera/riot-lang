open Std

module Frame = Http.Ws.Frame
module Parser = Http.Ws.Parser
module Serializer = Http.Ws.Serializer

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

let test_parse_preserves_remaining_frame = fun _ctx ->
  match Parser.parse ~role:Parser.Client "\x89\x00\x8a\x00" with
  | Parser.Done { value = { Frame.opcode = Frame.Ping; _ }; remaining } when remaining = "\x8a\x00" -> (
      match Parser.parse ~role:Parser.Client remaining with
      | Parser.Done {
          value = {
            Frame.opcode = Frame.Pong;
            fin = true;
            payload = "";
            _;
          };
          remaining = "";
        } ->
          Result.Ok ()
      | Parser.Done _ -> Result.Error "remaining frame parsed with the wrong shape"
      | Parser.Need_more -> Result.Error "remaining frame unexpectedly needed more data"
      | Parser.Error err ->
          Result.Error ("remaining frame was rejected: " ^ Parser.error_to_string err)
    )
  | Parser.Done _ -> Result.Error "first frame did not preserve the next frame as remaining bytes"
  | Parser.Need_more -> Result.Error "two-frame input unexpectedly needed more data"
  | Parser.Error err -> Result.Error ("two-frame input was rejected: " ^ Parser.error_to_string err)

let test_parse_valid_masked_client_ping = fun _ctx ->
  match Parser.parse ~role:Parser.Server "\x89\x80\x00\x00\x00\x00" with
  | Parser.Done {
      value = {
        Frame.opcode = Frame.Ping;
        fin = true;
        masked = true;
        payload = "";
        _;
      };
      remaining = "";
    } ->
      Result.Ok ()
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

let test_parse_rejects_non_minimal_16_bit_length = fun _ctx ->
  expect_parse_error
    ~role:Parser.Client
    "\x81\x7e\x00\x7d"
    (Parser.NonMinimalPayloadLength { encoding = Parser.PayloadLength16; payload_length = 125 })

let test_parse_rejects_non_minimal_64_bit_length = fun _ctx ->
  expect_parse_error
    ~role:Parser.Client
    "\x82\x7f\x00\x00\x00\x00\x00\x00\xff\xff"
    (Parser.NonMinimalPayloadLength { encoding = Parser.PayloadLength64; payload_length = 65_535 })

let test_parse_rejects_payload_over_limit = fun _ctx ->
  match Parser.parse ~max_payload_length:2 ~role:Parser.Client "\x81\x03abc" with
  | Parser.Error (
    Parser.PayloadLengthExceedsLimit { payload_length = 3; max_payload_length = 2 }
  ) ->
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

let test_parse_valid_close_with_reason = fun _ctx ->
  match Parser.parse ~role:Parser.Client "\x88\x05\x03\xe8bye" with
  | Parser.Done { value = { Frame.opcode = Frame.Close; payload = "\x03\xe8bye"; _ }; remaining = "" } ->
      Result.Ok ()
  | Parser.Done _ -> Result.Error "Close frame parsed with the wrong shape"
  | Parser.Need_more -> Result.Error "Close frame unexpectedly needed more data"
  | Parser.Error err -> Result.Error ("Close frame was rejected: " ^ Parser.error_to_string err)

let test_parse_rejects_one_byte_close_payload = fun _ctx ->
  expect_parse_error
    ~role:Parser.Client
    "\x88\x01\x03"
    (Parser.ClosePayloadTooShort { payload_length = 1 })

let test_parse_rejects_invalid_close_code = fun _ctx ->
  expect_parse_error
    ~role:Parser.Client
    "\x88\x02\x03\xe7"
    (Parser.InvalidCloseCode { code = 999 })

let test_parse_rejects_invalid_close_reason_utf8 = fun _ctx ->
  expect_parse_error
    ~role:Parser.Client
    "\x88\x04\x03\xe8\xc0\x80"
    (Parser.InvalidCloseReasonUtf8 { reason_length = 2 })

let test_parse_rejects_invalid_text_utf8 = fun _ctx ->
  expect_parse_error
    ~role:Parser.Client
    "\x81\x02\xc0\x80"
    (Parser.InvalidTextPayloadUtf8 { payload_length = 2 })

let fixed_mask_rng = fun () ->
  let mask_bytes = "\x04\x03\x02\x01" in
  Random.Rng.make
    ~state:()
    ~fill_bytes:(fun () out ->
      for index = 0 to IO.Bytes.length out - 1 do
        IO.Bytes.set_unchecked
          out
          ~at:index
          ~char:(String.get_unchecked mask_bytes ~at:(index mod 4))
      done)

let test_serialize_unmasked_frame = fun _ctx ->
  match Serializer.serialize (Frame.ping ()) with
  | Ok "\x89\x00" -> Result.Ok ()
  | Ok _ -> Result.Error "PING frame serialized with the wrong bytes"
  | Error error ->
      Result.Error ("PING frame serialization failed: " ^ Serializer.error_to_string error)

let test_serialize_masked_frame_uses_rng = fun _ctx ->
  let frame = Frame.{ (text "Hi") with masked = true } in
  match Serializer.serialize ~rng:(fixed_mask_rng ()) ~role:Serializer.Client frame with
  | Ok "\x81\x82\x01\x02\x03\x04Ik" -> Result.Ok ()
  | Ok _ -> Result.Error "masked frame serialized with the wrong mask or payload"
  | Error error ->
      Result.Error ("masked frame serialization failed: " ^ Serializer.error_to_string error)

let expect_serialize_error = fun frame expected ->
  match Serializer.serialize frame with
  | Error error when error = expected -> Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "Expected serializer error"

let test_serialize_rejects_rsv_bits = fun _ctx ->
  expect_serialize_error
    Frame.{ (text "x") with rsv1 = true }
    Serializer.ReservedBitsSet

let test_serialize_rejects_unmasked_client_frame = fun _ctx ->
  match Serializer.serialize ~role:Serializer.Client (Frame.text "x") with
  | Error Serializer.ClientFrameNotMasked -> Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "Expected serializer error"

let test_serialize_rejects_masked_server_frame = fun _ctx ->
  expect_serialize_error
    Frame.{ (text "x") with masked = true }
    Serializer.ServerFrameMasked

let test_serialize_rejects_fragmented_control_frame = fun _ctx ->
  expect_serialize_error
    Frame.{ (ping ()) with fin = false }
    (Serializer.FragmentedControlFrame { opcode = Frame.Ping })

let test_serialize_rejects_oversized_control_frame = fun _ctx ->
  let payload = String.make ~len:126 ~char:'x' in
  expect_serialize_error
    (Frame.ping ~payload ())
    (Serializer.ControlFramePayloadTooLarge { opcode = Frame.Ping; payload_length = 126 })

let test_serialize_rejects_invalid_close_payload = fun _ctx ->
  expect_serialize_error
    (Frame.close ~payload:"\x03\xed" ())
    (Serializer.InvalidClosePayload (Frame.InvalidCloseCode { code = 1_005 }))

let test_serialize_rejects_invalid_text_utf8 = fun _ctx ->
  expect_serialize_error
    (Frame.text "\xc0\x80")
    (Serializer.InvalidTextPayloadUtf8 { payload_length = 2 })

let tests =
  Test.[
    case "parse_valid_ping" test_parse_valid_ping;
    case "parse_preserves_remaining_frame" test_parse_preserves_remaining_frame;
    case "parse_valid_masked_client_ping" test_parse_valid_masked_client_ping;
    case "parse_rejects_unmasked_client_frame" test_parse_rejects_unmasked_client_frame;
    case "parse_rejects_masked_server_frame" test_parse_rejects_masked_server_frame;
    case "parse_rejects_rsv_bits" test_parse_rejects_rsv_bits;
    case "parse_rejects_fragmented_ping" test_parse_rejects_fragmented_ping;
    case "parse_rejects_oversized_control_length" test_parse_rejects_oversized_control_length;
    case "parse_64_bit_length_uses_high_bytes" test_parse_64_bit_length_uses_high_bytes;
    case "parse_rejects_64_bit_length_high_bit" test_parse_rejects_64_bit_length_high_bit;
    case "parse_rejects_64_bit_length_above_int_max" test_parse_rejects_64_bit_length_above_int_max;
    case "parse_rejects_non_minimal_16_bit_length" test_parse_rejects_non_minimal_16_bit_length;
    case "parse_rejects_non_minimal_64_bit_length" test_parse_rejects_non_minimal_64_bit_length;
    case "parse_rejects_payload_over_limit" test_parse_rejects_payload_over_limit;
    case "parse_rejects_negative_payload_limit" test_parse_rejects_negative_payload_limit;
    case "parse_valid_close_with_reason" test_parse_valid_close_with_reason;
    case "parse_rejects_one_byte_close_payload" test_parse_rejects_one_byte_close_payload;
    case "parse_rejects_invalid_close_code" test_parse_rejects_invalid_close_code;
    case "parse_rejects_invalid_close_reason_utf8" test_parse_rejects_invalid_close_reason_utf8;
    case "parse_rejects_invalid_text_utf8" test_parse_rejects_invalid_text_utf8;
    case "serialize_unmasked_frame" test_serialize_unmasked_frame;
    case "serialize_masked_frame_uses_rng" test_serialize_masked_frame_uses_rng;
    case "serialize_rejects_rsv_bits" test_serialize_rejects_rsv_bits;
    case "serialize_rejects_unmasked_client_frame" test_serialize_rejects_unmasked_client_frame;
    case "serialize_rejects_masked_server_frame" test_serialize_rejects_masked_server_frame;
    case
      "serialize_rejects_fragmented_control_frame"
      test_serialize_rejects_fragmented_control_frame;
    case "serialize_rejects_oversized_control_frame" test_serialize_rejects_oversized_control_frame;
    case "serialize_rejects_invalid_close_payload" test_serialize_rejects_invalid_close_payload;
    case "serialize_rejects_invalid_text_utf8" test_serialize_rejects_invalid_text_utf8;
  ]

let main ~args:_ = Test.Cli.main ~name:"http:ws_parser" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
