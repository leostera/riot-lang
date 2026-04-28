open Std

module Hpack = Http.Http2.Hpack

let encode_or_error = fun encoder ~sensitive_headers ~headers ->
  match Hpack.encode encoder ~sensitive_headers () ~headers with
  | Ok encoded -> Ok encoded
  | Error err -> Error ("Encode failed: " ^ Hpack.encode_error_to_string err)

let test_encoder_decoder_roundtrip = fun _ctx ->
  let encoder = Hpack.create_encoder () in
  let decoder = Hpack.create_decoder () in
  let headers = [
    { Hpack.name = "content-type"; value = "application/json" };
    { Hpack.name = "content-length"; value = "123" };
  ]
  in
  match encode_or_error encoder ~sensitive_headers:[] ~headers with
  | Error err -> Result.Error err
  | Ok encoded ->
      let decoded = Hpack.decode decoder encoded in
      match decoded with
      | Ok decoded_headers ->
          if List.length decoded_headers = List.length headers then
            Result.Ok ()
          else
            Result.Error ("Header count mismatch: expected "
            ^ Int.to_string (List.length headers)
            ^ ", got "
            ^ Int.to_string (List.length decoded_headers))
      | Error err -> Result.Error ("Decode failed: " ^ Hpack.decode_error_to_string err)

let test_static_table_lookup = fun _ctx ->
  match Hpack.static_table_lookup 2 with
  | Some header ->
      if header.name = ":method" && header.value = "GET" then
        Result.Ok ()
      else
        Result.Error ("Static table entry 2 has wrong values: " ^ header.name ^ "=" ^ header.value)
  | None -> Result.Error "Static table lookup failed"

let test_encode_simple_header = fun _ctx ->
  let encoder = Hpack.create_encoder () in
  let headers = [ { Hpack.name = ":method"; value = "GET" } ] in
  match encode_or_error encoder ~sensitive_headers:[] ~headers with
  | Error err -> Result.Error err
  | Ok encoded ->
      if IO.Bytes.length encoded > 0 then
        Result.Ok ()
      else
        Result.Error "Encoding produced empty output"

let test_custom_header_name_roundtrip = fun _ctx ->
  let encoder = Hpack.create_encoder () in
  let decoder = Hpack.create_decoder () in
  let headers = [ { Hpack.name = "x-request-id"; value = "req-123" } ] in
  match encode_or_error encoder ~sensitive_headers:[] ~headers with
  | Error err -> Result.Error err
  | Ok encoded ->
      match Hpack.decode decoder encoded with
      | Ok [ { Hpack.name = "x-request-id"; value = "req-123" } ] -> Result.Ok ()
      | Ok _ -> Result.Error "Custom header name did not roundtrip"
      | Error err -> Result.Error ("Decode failed: " ^ Hpack.decode_error_to_string err)

let test_sensitive_custom_header_name_roundtrip = fun _ctx ->
  let encoder = Hpack.create_encoder () in
  let decoder = Hpack.create_decoder () in
  let headers = [ { Hpack.name = "x-secret"; value = "token" } ] in
  match encode_or_error encoder ~sensitive_headers:[ "x-secret"; ] ~headers with
  | Error err -> Result.Error err
  | Ok encoded ->
      match Hpack.decode decoder encoded with
      | Ok [ { Hpack.name = "x-secret"; value = "token" } ] -> Result.Ok ()
      | Ok _ -> Result.Error "Sensitive custom header name did not roundtrip"
      | Error err -> Result.Error ("Decode failed: " ^ Hpack.decode_error_to_string err)

let test_literal_with_indexing_updates_decoder_table = fun _ctx ->
  let encoder = Hpack.create_encoder () in
  let decoder = Hpack.create_decoder () in
  let headers = [
    { Hpack.name = "x-request-id"; value = "req-123" };
    { Hpack.name = "x-request-id"; value = "req-123" };
  ]
  in
  match encode_or_error encoder ~sensitive_headers:[] ~headers with
  | Error err -> Result.Error err
  | Ok encoded ->
      match Hpack.decode decoder encoded with
      | Ok [ { Hpack.name = "x-request-id"; value = "req-123" }; { Hpack.name = "x-request-id"; value = "req-123" } ] ->
          Result.Ok ()
      | Ok _ -> Result.Error "Decoder dynamic table did not preserve repeated indexed header"
      | Error err -> Result.Error ("Decode failed: " ^ Hpack.decode_error_to_string err)

let test_huffman_header_name_is_rejected = fun _ctx ->
  let decoder = Hpack.create_decoder () in
  (* Literal with indexing, literal name length=1 with Huffman bit set, value "b". *)
  let encoded = IO.Bytes.from_string "\x40\x81a\x01b" in
  match Hpack.decode decoder encoded with
  | Error Hpack.UnsupportedHuffmanStringEncoding -> Result.Ok ()
  | Error err -> Result.Error ("Wrong decode error: " ^ Hpack.decode_error_to_string err)
  | Ok _ -> Result.Error "Huffman-encoded header name decoded as plain text"

let test_huffman_header_value_is_rejected = fun _ctx ->
  let decoder = Hpack.create_decoder () in
  (* Literal with indexing, name "a", literal value length=1 with Huffman bit set. *)
  let encoded = IO.Bytes.from_string "\x40\x01a\x81b" in
  match Hpack.decode decoder encoded with
  | Error Hpack.UnsupportedHuffmanStringEncoding -> Result.Ok ()
  | Error err -> Result.Error ("Wrong decode error: " ^ Hpack.decode_error_to_string err)
  | Ok _ -> Result.Error "Huffman-encoded header value decoded as plain text"

let test_indexed_missing_header_returns_typed_error = fun _ctx ->
  let encoder = Hpack.create_encoder () in
  let header = { Hpack.name = "x-missing"; value = "value" } in
  match Hpack.encode_header encoder header ~encoding_type:Hpack.Indexed with
  | Error (Hpack.HeaderNotIndexed got) when got = header -> Result.Ok ()
  | Error err -> Result.Error ("Wrong encode error: " ^ Hpack.encode_error_to_string err)
  | Ok _ -> Result.Error "Missing indexed header encoded as a literal"

let tests = [
  Test.case "encoder_decoder_roundtrip" test_encoder_decoder_roundtrip;
  Test.case "static_table_lookup" test_static_table_lookup;
  Test.case "encode_simple_header" test_encode_simple_header;
  Test.case "custom_header_name_roundtrip" test_custom_header_name_roundtrip;
  Test.case "sensitive_custom_header_name_roundtrip" test_sensitive_custom_header_name_roundtrip;
  Test.case
    "literal_with_indexing_updates_decoder_table"
    test_literal_with_indexing_updates_decoder_table;
  Test.case "huffman_header_name_is_rejected" test_huffman_header_name_is_rejected;
  Test.case "huffman_header_value_is_rejected" test_huffman_header_value_is_rejected;
  Test.case
    "indexed_missing_header_returns_typed_error"
    test_indexed_missing_header_returns_typed_error;
]

let main ~args:_ = Test.Cli.main ~name:"http:hpack" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
