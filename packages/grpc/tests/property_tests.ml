open Std
open Propane

(** Property-based tests for gRPC protocol implementation *)

(** {1 Message Framing Properties} *)

(* Generator for random bytes *)
let bytes_gen =
  Generator.map
    IO.Bytes.of_string
    (Generator.string_of Generator.char_printable)

(* Generator for small bytes (to avoid hitting size limits) *)
let small_bytes_gen =
  Generator.map
    IO.Bytes.of_string
    (Generator.string_size (Generator.int_range 0 1000) Generator.char_printable)

(* Generator for boolean (compressed flag) *)
let bool_gen = Generator.bool

(* Property: encode >> decode = identity for any valid payload *)
let message_roundtrip_prop =
  property "Message encode/decode round-trip preserves data"
    Arbitrary.(pair (make small_bytes_gen) (make bool_gen))
    (fun (payload, compressed) ->
      let encoded = Grpc.Message.encode ~compressed ~payload in
      match Grpc.Message.decode encoded with
      | Result.Error _ -> false
      | Result.Ok (msg, remaining) ->
          IO.Bytes.equal msg.payload payload
          && msg.compressed = compressed
          && IO.Bytes.length remaining = 0)

(* Property: encoded message length is always 5 + payload length *)
let message_length_prop =
  property "Encoded message length is always 5 + payload length"
    Arbitrary.(make small_bytes_gen)
    (fun payload ->
      let encoded = Grpc.Message.encode ~compressed:false ~payload in
      IO.Bytes.length encoded = 5 + IO.Bytes.length payload)

(* Property: compression flag is preserved *)
let message_compression_preserved_prop =
  property "Compression flag is preserved through encode/decode"
    Arbitrary.(pair (make small_bytes_gen) (make bool_gen))
    (fun (payload, compressed) ->
      let encoded = Grpc.Message.encode ~compressed ~payload in
      match Grpc.Message.decode encoded with
      | Result.Error _ -> false
      | Result.Ok (msg, _) -> msg.compressed = compressed)

(* Property: decode never crashes on random input *)
let message_decode_never_crashes_prop =
  property "Message decode never crashes on random input"
    Arbitrary.(make bytes_gen)
    (fun bytes ->
      match Grpc.Message.decode bytes with
      | Result.Ok _ -> true
      | Result.Error _ -> true)

(* Property: empty payload round-trips correctly *)
let message_empty_payload_prop =
  property "Empty payload encodes/decodes correctly"
    Arbitrary.(make bool_gen)
    (fun compressed ->
      let payload = IO.Bytes.of_string "" in
      let encoded = Grpc.Message.encode ~compressed ~payload in
      match Grpc.Message.decode encoded with
      | Result.Error _ -> false
      | Result.Ok (msg, _) ->
          IO.Bytes.length msg.payload = 0 && msg.compressed = compressed)

(* Property: peek_header returns correct information *)
let message_peek_header_prop =
  property "peek_header returns correct compression and length"
    Arbitrary.(pair (make small_bytes_gen) (make bool_gen))
    (fun (payload, compressed) ->
      let encoded = Grpc.Message.encode ~compressed ~payload in
      match Grpc.Message.peek_header encoded with
      | Result.Error _ -> false
      | Result.Ok (comp_flag, length) ->
          comp_flag = compressed && length = IO.Bytes.length payload)

(** {1 Status Properties} *)

(* Generator for valid status codes (0-16) *)
let status_code_gen = Generator.int_range 0 16

(* Property: All status codes round-trip through to_int/of_int *)
let status_roundtrip_prop =
  property "Status codes round-trip through to_int/of_int"
    Arbitrary.(make status_code_gen)
    (fun code ->
      match Grpc.Status.of_int code with
      | None -> false
      | Some status -> Grpc.Status.to_int status = code)

(* Property: to_http_status never crashes *)
let status_to_http_never_crashes_prop =
  property "to_http_status never crashes"
    Arbitrary.(make status_code_gen)
    (fun code ->
      match Grpc.Status.of_int code with
      | None -> true
      | Some status ->
          let _ = Grpc.Status.to_http_status status in
          true)

(* Property: is_retriable is consistent *)
let status_retriable_consistent_prop =
  property "is_retriable is consistent for same status"
    Arbitrary.(make status_code_gen)
    (fun code ->
      match Grpc.Status.of_int code with
      | None -> true
      | Some status ->
          let r1 = Grpc.Status.is_retriable status in
          let r2 = Grpc.Status.is_retriable status in
          r1 = r2)

(** {1 Metadata Properties} *)

(* Generator for valid header keys (lowercase, no special chars) *)
let header_key_gen =
  Generator.map
    String.lowercase_ascii
    (Generator.string_size 
      (Generator.int_range 1 20)
      (Generator.char_range 'a' 'z'))

(* Generator for header values *)
let header_value_gen =
  Generator.string_size 
    (Generator.int_range 0 100)
    Generator.char_printable

(* Property: Metadata add/get round-trip *)
let metadata_roundtrip_prop =
  property "Metadata add/get preserves values"
    Arbitrary.(pair (make header_key_gen) (make header_value_gen))
    (fun (key, value) ->
      let meta = Grpc.Metadata.empty |> Grpc.Metadata.add ~key ~value in
      match Grpc.Metadata.get meta ~key with
      | None -> false
      | Some v -> String.equal v value)

(* Property: Binary encoding round-trip *)
let metadata_binary_roundtrip_prop =
  property "Binary metadata encoding round-trips"
    Arbitrary.(make bytes_gen)
    (fun data ->
      let encoded = Grpc.Metadata.encode_binary data in
      match Grpc.Metadata.decode_binary encoded with
      | Result.Error _ -> false
      | Result.Ok decoded -> IO.Bytes.equal data decoded)

(* Property: is_binary correctly identifies binary keys *)
let metadata_is_binary_prop =
  property "is_binary correctly identifies -bin suffix"
    Arbitrary.(make header_key_gen)
    (fun key ->
      let binary_key = key ^ "-bin" in
      let text_key = key in
      Grpc.Metadata.is_binary binary_key && not (Grpc.Metadata.is_binary text_key))

(* Property: Timeout encoding creates valid format *)
let metadata_timeout_format_prop =
  property "Timeout encoding creates parseable format"
    Arbitrary.(make (Generator.int_range 1 1000))
    (fun value ->
      let timeout = { Grpc.Metadata.value; unit = `Seconds } in
      let (_key, encoded) = Grpc.Metadata.timeout timeout in
      (* Should be able to parse it back *)
      match Grpc.Metadata.parse_timeout encoded with
      | None -> false
      | Some parsed -> parsed.value = value && parsed.unit = `Seconds)

(** {1 Test Suite} *)

let tests =
  Test.[
    (* Message framing properties *)
    message_roundtrip_prop;
    message_length_prop;
    message_compression_preserved_prop;
    message_decode_never_crashes_prop;
    message_empty_payload_prop;
    message_peek_header_prop;
    (* Status properties *)
    status_roundtrip_prop;
    status_to_http_never_crashes_prop;
    status_retriable_consistent_prop;
    (* Metadata properties *)
    metadata_roundtrip_prop;
    metadata_binary_roundtrip_prop;
    metadata_is_binary_prop;
    metadata_timeout_format_prop;
  ]

let () =
  Miniriot.run ~main:(fun ~args:_ ->
      Test.Cli.main ~name:"grpc:property" ~tests ~args:Env.args)
    ~args:Env.args ()
