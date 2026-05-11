open Std

module Hpack = Http.Http2.Hpack
module HpackReader = Http.Http2.Hpack_reader

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

let test_sensitive_header_name_matching_is_case_insensitive = fun _ctx ->
  if not (Hpack.is_sensitive_header "Authorization") then
    Result.Error "Authorization was not treated as sensitive"
  else if not (Hpack.is_sensitive_header "Cookie") then
    Result.Error "Cookie was not treated as sensitive"
  else if not (Hpack.is_sensitive_header "Proxy-Authorization") then
    Result.Error "Proxy-Authorization was not treated as sensitive"
  else
    Result.Ok ()

let test_uppercase_sensitive_header_uses_never_indexed = fun _ctx ->
  let encoder = Hpack.create_encoder () in
  let decoder = Hpack.create_decoder () in
  let headers = [ { Hpack.name = "Authorization"; value = "Bearer secret" } ] in
  match encode_or_error encoder ~sensitive_headers:[] ~headers with
  | Error err -> Result.Error err
  | Ok encoded ->
      let encoded_string = IO.Bytes.to_string encoded in
      let first_byte = Char.to_int (String.get_unchecked encoded_string ~at:0) in
      if first_byte land 0b1111_0000 != 0b0001_0000 then
        Result.Error "Authorization was not encoded with the never-indexed representation"
      else
        match Hpack.decode decoder encoded with
        | Ok [ { Hpack.name = "authorization"; value = "Bearer secret" } ] -> Result.Ok ()
        | Ok _ -> Result.Error "Uppercase sensitive header did not decode as lowercase"
        | Error err -> Result.Error ("Decode failed: " ^ Hpack.decode_error_to_string err)

let test_explicit_never_indexed_ignores_dynamic_exact_match = fun _ctx ->
  let encoder = Hpack.create_encoder () in
  let header = { Hpack.name = "x-secret"; value = "token" } in
  match Hpack.encode_header encoder header ~encoding_type:Hpack.LiteralWithIndexing with
  | Error err -> Result.Error ("Initial encode failed: " ^ Hpack.encode_error_to_string err)
  | Ok _ ->
      match Hpack.encode_header encoder header ~encoding_type:Hpack.LiteralNeverIndexed with
      | Error err ->
          Result.Error ("Never-indexed encode failed: " ^ Hpack.encode_error_to_string err)
      | Ok encoded ->
          let encoded_string = IO.Bytes.to_string encoded in
          let first_byte = Char.to_int (String.get_unchecked encoded_string ~at:0) in
          if first_byte land 0b1000_0000 = 0b1000_0000 then
            Result.Error "Explicit never-indexed header reused the indexed representation"
          else if first_byte land 0b1111_0000 != 0b0001_0000 then
            Result.Error "Explicit never-indexed header used the wrong literal representation"
          else
            Result.Ok ()

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
      | Ok [
          {
            Hpack.name = "x-request-id";
            value = "req-123";
          };
          {
            Hpack.name = "x-request-id";
            value = "req-123";
          };
        ] -> Result.Ok ()
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

let test_integer_decode_overflow_returns_typed_error = fun _ctx ->
  let decoder = Hpack.create_decoder () in
  let encoded = IO.Bytes.from_string ("\xff" ^ String.make ~len:20 ~char:'\xff') in
  match Hpack.decode decoder encoded with
  | Error (Hpack.IntegerEncodingOverflow _) -> Result.Ok ()
  | Error err -> Result.Error ("Wrong decode error: " ^ Hpack.decode_error_to_string err)
  | Ok _ -> Result.Error "Overflowing HPACK integer decoded successfully"

let test_integer_decode_zero_continuation_returns_typed_error = fun _ctx ->
  let decoder = Hpack.create_decoder () in
  let encoded = IO.Bytes.from_string "\x0f\x00\x52\x00\x52" in
  match Hpack.decode decoder encoded with
  | Error (Hpack.StringDataTruncated { length = 82; available = 2 }) -> Result.Ok ()
  | Error err -> Result.Error ("Wrong decode error: " ^ Hpack.decode_error_to_string err)
  | Ok _ -> Result.Error "Truncated HPACK literal decoded successfully"

let test_indexed_missing_header_returns_typed_error = fun _ctx ->
  let encoder = Hpack.create_encoder () in
  let header = { Hpack.name = "x-missing"; value = "value" } in
  match Hpack.encode_header encoder header ~encoding_type:Hpack.Indexed with
  | Error (Hpack.HeaderNotIndexed got) when got = header -> Result.Ok ()
  | Error err -> Result.Error ("Wrong encode error: " ^ Hpack.encode_error_to_string err)
  | Ok _ -> Result.Error "Missing indexed header encoded as a literal"

let test_encoder_table_size_rejects_negative_update = fun _ctx ->
  let encoder = Hpack.create_encoder () in
  match Hpack.update_encoder_max_table_size encoder (-1) with
  | Error (Hpack.InvalidTableSize { size }) when Int.equal size (-1) -> Result.Ok ()
  | Error err -> Result.Error ("Wrong table size error: " ^ Hpack.table_size_error_to_string err)
  | Ok () -> Result.Error "Negative encoder table size update was accepted"

let test_decoder_table_size_rejects_negative_update = fun _ctx ->
  let decoder = Hpack.create_decoder () in
  match Hpack.update_decoder_max_table_size decoder (-1) with
  | Error (Hpack.InvalidTableSize { size }) when Int.equal size (-1) -> Result.Ok ()
  | Error err -> Result.Error ("Wrong table size error: " ^ Hpack.table_size_error_to_string err)
  | Ok () -> Result.Error "Negative decoder table size update was accepted"

let test_header_block_table_size_update = fun _ctx ->
  let decoder = Hpack.create_decoder () in
  let encoded = IO.Bytes.from_string "\x3f\xe1\x1f" in
  match Hpack.decode decoder encoded with
  | Ok [] -> Result.Ok ()
  | Ok _ -> Result.Error "Dynamic table size update produced headers"
  | Error err -> Result.Error ("Decode failed: " ^ Hpack.decode_error_to_string err)

let test_header_block_allows_table_size_update_before_headers = fun _ctx ->
  let decoder = Hpack.create_decoder () in
  let encoded = IO.Bytes.from_string "\x20\x82" in
  match Hpack.decode decoder encoded with
  | Ok [ { Hpack.name = ":method"; value = "GET" } ] -> Result.Ok ()
  | Ok _ -> Result.Error "Header block table size update decoded the wrong headers"
  | Error err -> Result.Error ("Decode failed: " ^ Hpack.decode_error_to_string err)

let test_header_block_rejects_table_size_update_after_headers = fun _ctx ->
  let decoder = Hpack.create_decoder () in
  let encoded = IO.Bytes.from_string "\x82\x20" in
  match Hpack.decode decoder encoded with
  | Error Hpack.DynamicTableSizeUpdateAfterHeaders -> Result.Ok ()
  | Error err -> Result.Error ("Wrong decode error: " ^ Hpack.decode_error_to_string err)
  | Ok _ -> Result.Error "Dynamic table size update after a header field was accepted"

let test_literal_without_indexing_does_not_update_decoder_table = fun _ctx ->
  let decoder = Hpack.create_decoder () in
  let encoded = IO.Bytes.from_string "\x00\x01x\x01y\xbe" in
  match Hpack.decode decoder encoded with
  | Error (Hpack.InvalidHeaderIndex 62) -> Result.Ok ()
  | Error err -> Result.Error ("Wrong decode error: " ^ Hpack.decode_error_to_string err)
  | Ok _ -> Result.Error "Literal without indexing was added to the dynamic table"

let test_literal_never_indexed_does_not_update_decoder_table = fun _ctx ->
  let decoder = Hpack.create_decoder () in
  let encoded = IO.Bytes.from_string "\x10\x01x\x01y\xbe" in
  match Hpack.decode decoder encoded with
  | Error (Hpack.InvalidHeaderIndex 62) -> Result.Ok ()
  | Error err -> Result.Error ("Wrong decode error: " ^ Hpack.decode_error_to_string err)
  | Ok _ -> Result.Error "Literal never indexed was added to the dynamic table"

let test_decoder_dynamic_table_evicts_oversized_literal = fun _ctx ->
  let decoder = Hpack.create_decoder ~max_dynamic_table_size:33 () in
  let encoded = IO.Bytes.from_string "\x40\x01x\x01y" in
  match Hpack.decode decoder encoded with
  | Ok [ { Hpack.name = "x"; value = "y" } ] ->
      if Hpack.decoder_dynamic_table_size decoder = 0 then
        Result.Ok ()
      else
        Result.Error ("Oversized indexed literal left "
        ^ Int.to_string (Hpack.decoder_dynamic_table_size decoder)
        ^ " bytes in the dynamic table")
  | Ok _ -> Result.Error "Oversized indexed literal decoded the wrong headers"
  | Error err -> Result.Error ("Decode failed: " ^ Hpack.decode_error_to_string err)

let test_encoder_dynamic_table_size_tracks_indexed_literals = fun _ctx ->
  let encoder = Hpack.create_encoder () in
  let header = { Hpack.name = "x"; value = "y" } in
  match Hpack.encode_header encoder header ~encoding_type:Hpack.LiteralWithIndexing with
  | Error err -> Result.Error ("Encode failed: " ^ Hpack.encode_error_to_string err)
  | Ok _ ->
      let expected = Hpack.header_size header in
      let actual = Hpack.encoder_dynamic_table_size encoder in
      if Int.equal actual expected then
        Result.Ok ()
      else
        Result.Error ("Encoder dynamic table size mismatch: expected "
        ^ Int.to_string expected
        ^ ", got "
        ^ Int.to_string actual)

let test_reader_decodes_static_indexed_header = fun _ctx ->
  let decoder = HpackReader.create () in
  match HpackReader.decode decoder (IO.Reader.from_string "\x82") with
  | HpackReader.Headers [ { Hpack.name = ":method"; value = "GET" } ] -> Result.Ok ()
  | HpackReader.Headers _ -> Result.Error "Reader decoded the wrong static header"
  | HpackReader.Need_more -> Result.Error "Reader unexpectedly needed more data"
  | HpackReader.Error err ->
      Result.Error ("Reader failed: " ^ HpackReader.decode_error_to_string err)

let test_reader_preserves_partial_literal = fun _ctx ->
  let decoder = HpackReader.create () in
  match HpackReader.decode decoder (IO.Reader.from_string "\x40\x01x") with
  | HpackReader.Need_more -> (
      match HpackReader.decode decoder (IO.Reader.from_string "\x03one") with
      | HpackReader.Headers [ { Hpack.name = "x"; value = "one" } ] -> Result.Ok ()
      | HpackReader.Headers _ -> Result.Error "Reader decoded the wrong resumed literal"
      | HpackReader.Need_more ->
          Result.Error "Reader still needed more data after literal completion"
      | HpackReader.Error err ->
          Result.Error ("Reader failed after resume: " ^ HpackReader.decode_error_to_string err)
    )
  | HpackReader.Headers _ -> Result.Error "Partial literal decoded before value bytes arrived"
  | HpackReader.Error err ->
      Result.Error ("Partial literal failed: " ^ HpackReader.decode_error_to_string err)

let test_reader_decodes_literal_without_indexing = fun _ctx ->
  let decoder = HpackReader.create () in
  match HpackReader.decode decoder (IO.Reader.from_string "\x00\x01x\x01y") with
  | HpackReader.Headers [ { Hpack.name = "x"; value = "y" } ] -> Result.Ok ()
  | HpackReader.Headers _ -> Result.Error "Reader decoded the wrong literal without indexing"
  | HpackReader.Need_more -> Result.Error "Reader unexpectedly needed more data"
  | HpackReader.Error err ->
      Result.Error ("Reader failed: " ^ HpackReader.decode_error_to_string err)

let test_reader_reuses_dynamic_table = fun _ctx ->
  let decoder = HpackReader.create () in
  match HpackReader.decode decoder (IO.Reader.from_string "\x40\x01x\x01y") with
  | HpackReader.Headers [ { Hpack.name = "x"; value = "y" } ] -> (
      match HpackReader.decode decoder (IO.Reader.from_string "\xbe") with
      | HpackReader.Headers [ { Hpack.name = "x"; value = "y" } ] -> Result.Ok ()
      | HpackReader.Headers _ -> Result.Error "Reader decoded the wrong dynamic-table header"
      | HpackReader.Need_more -> Result.Error "Reader unexpectedly needed more dynamic index data"
      | HpackReader.Error err ->
          Result.Error ("Reader dynamic-table decode failed: "
          ^ HpackReader.decode_error_to_string err)
    )
  | HpackReader.Headers _ -> Result.Error "Reader decoded the wrong indexed literal"
  | HpackReader.Need_more -> Result.Error "Reader unexpectedly needed more indexed literal data"
  | HpackReader.Error err ->
      Result.Error ("Reader indexed literal failed: " ^ HpackReader.decode_error_to_string err)

let test_reader_dynamic_table_size_tracks_indexed_literals = fun _ctx ->
  let decoder = HpackReader.create () in
  if HpackReader.dynamic_table_size decoder != 0 then
    Result.Error "New HPACK reader reported a non-empty dynamic table"
  else
    match HpackReader.decode decoder (IO.Reader.from_string "\x40\x01x\x01y") with
    | HpackReader.Headers [ { Hpack.name = "x"; value = "y" } ] ->
        let actual = HpackReader.dynamic_table_size decoder in
        if Int.equal actual 34 then
          Result.Ok ()
        else
          Result.Error ("Reader dynamic table size mismatch: expected 34, got "
          ^ Int.to_string actual)
    | HpackReader.Headers _ -> Result.Error "Reader decoded the wrong indexed literal"
    | HpackReader.Need_more -> Result.Error "Reader unexpectedly needed more indexed literal data"
    | HpackReader.Error err ->
        Result.Error ("Reader indexed literal failed: " ^ HpackReader.decode_error_to_string err)

let test_reader_reset_clears_dynamic_table = fun _ctx ->
  let decoder = HpackReader.create () in
  match HpackReader.decode decoder (IO.Reader.from_string "\x40\x01x\x01y") with
  | HpackReader.Headers headers ->
      if headers != [ { Hpack.name = "x"; value = "y" } ] then
        Result.Error "Reader decoded the wrong indexed literal"
      else (
        HpackReader.reset decoder;
        if HpackReader.dynamic_table_size decoder != 0 then
          Result.Error "Reader reset did not clear the dynamic table"
        else
          match HpackReader.decode decoder (IO.Reader.from_string "\xbe") with
          | HpackReader.Error (
            HpackReader.HpackDecodeFailed (
              Hpack.InvalidHeaderIndex 62
            )
          ) ->
              Result.Ok ()
          | HpackReader.Error err ->
              Result.Error ("Wrong reader error after reset: "
              ^ HpackReader.decode_error_to_string err)
          | HpackReader.Need_more ->
              Result.Error "Reader treated invalid dynamic index as incomplete"
          | HpackReader.Headers _ -> Result.Error "Reader reused a dynamic header after reset"
      )
  | HpackReader.Need_more -> Result.Error "Reader unexpectedly needed more indexed literal data"
  | HpackReader.Error err ->
      Result.Error ("Reader indexed literal failed: " ^ HpackReader.decode_error_to_string err)

let test_reader_rejects_huffman_with_typed_error = fun _ctx ->
  let decoder = HpackReader.create () in
  match HpackReader.decode decoder (IO.Reader.from_string "\x40\x81a\x01b") with
  | HpackReader.Error (
    HpackReader.HpackDecodeFailed Hpack.UnsupportedHuffmanStringEncoding
  ) ->
      Result.Ok ()
  | HpackReader.Error err ->
      Result.Error ("Wrong reader error: " ^ HpackReader.decode_error_to_string err)
  | HpackReader.Need_more -> Result.Error "Reader treated Huffman input as incomplete"
  | HpackReader.Headers _ -> Result.Error "Reader decoded unsupported Huffman input"

let tests = [
  Test.case "encoder_decoder_roundtrip" test_encoder_decoder_roundtrip;
  Test.case "static_table_lookup" test_static_table_lookup;
  Test.case "encode_simple_header" test_encode_simple_header;
  Test.case "custom_header_name_roundtrip" test_custom_header_name_roundtrip;
  Test.case "sensitive_custom_header_name_roundtrip" test_sensitive_custom_header_name_roundtrip;
  Test.case
    "sensitive_header_name_matching_is_case_insensitive"
    test_sensitive_header_name_matching_is_case_insensitive;
  Test.case
    "uppercase_sensitive_header_uses_never_indexed"
    test_uppercase_sensitive_header_uses_never_indexed;
  Test.case
    "explicit_never_indexed_ignores_dynamic_exact_match"
    test_explicit_never_indexed_ignores_dynamic_exact_match;
  Test.case
    "literal_with_indexing_updates_decoder_table"
    test_literal_with_indexing_updates_decoder_table;
  Test.case "huffman_header_name_is_rejected" test_huffman_header_name_is_rejected;
  Test.case "huffman_header_value_is_rejected" test_huffman_header_value_is_rejected;
  Test.case
    "integer_decode_overflow_returns_typed_error"
    test_integer_decode_overflow_returns_typed_error;
  Test.case
    "integer_decode_zero_continuation_returns_typed_error"
    test_integer_decode_zero_continuation_returns_typed_error;
  Test.case
    "indexed_missing_header_returns_typed_error"
    test_indexed_missing_header_returns_typed_error;
  Test.case
    "encoder_table_size_rejects_negative_update"
    test_encoder_table_size_rejects_negative_update;
  Test.case
    "decoder_table_size_rejects_negative_update"
    test_decoder_table_size_rejects_negative_update;
  Test.case "header_block_table_size_update" test_header_block_table_size_update;
  Test.case
    "header_block_allows_table_size_update_before_headers"
    test_header_block_allows_table_size_update_before_headers;
  Test.case
    "header_block_rejects_table_size_update_after_headers"
    test_header_block_rejects_table_size_update_after_headers;
  Test.case
    "literal_without_indexing_does_not_update_decoder_table"
    test_literal_without_indexing_does_not_update_decoder_table;
  Test.case
    "literal_never_indexed_does_not_update_decoder_table"
    test_literal_never_indexed_does_not_update_decoder_table;
  Test.case
    "decoder_dynamic_table_evicts_oversized_literal"
    test_decoder_dynamic_table_evicts_oversized_literal;
  Test.case
    "encoder_dynamic_table_size_tracks_indexed_literals"
    test_encoder_dynamic_table_size_tracks_indexed_literals;
  Test.case "reader_decodes_static_indexed_header" test_reader_decodes_static_indexed_header;
  Test.case "reader_preserves_partial_literal" test_reader_preserves_partial_literal;
  Test.case "reader_decodes_literal_without_indexing" test_reader_decodes_literal_without_indexing;
  Test.case "reader_reuses_dynamic_table" test_reader_reuses_dynamic_table;
  Test.case
    "reader_dynamic_table_size_tracks_indexed_literals"
    test_reader_dynamic_table_size_tracks_indexed_literals;
  Test.case "reader_reset_clears_dynamic_table" test_reader_reset_clears_dynamic_table;
  Test.case "reader_rejects_huffman_with_typed_error" test_reader_rejects_huffman_with_typed_error;
]

let main ~args:_ = Test.Cli.main ~name:"http:hpack" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
