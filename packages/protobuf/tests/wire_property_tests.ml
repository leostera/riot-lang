open Std
open Propane

(** Property tests for Protobuf WireFormat *)

module WF = Protobuf.WireFormat

(* Generator for random bytes *)
let bytes_gen =
  Generator.map
    IO.Bytes.of_string
    (Generator.string_of Generator.char_printable)

(* Property: Decoding should never crash (always returns a result) *)
let decode_never_crashes_prop =
  property "WireFormat decode never crashes on random input"
    Arbitrary.(make bytes_gen)
    (fun bytes ->
      (* Just verify decode returns a result, don't care if Ok or Error *)
      match WF.decode bytes with
      | Result.Ok _ -> true
      | Result.Error _ -> true)

(* Property: Empty bytes should decode successfully *)
let empty_bytes_decode_prop =
  property "WireFormat decode succeeds on empty bytes"
    Arbitrary.(make (Generator.return (IO.Bytes.of_string "")))
    (fun bytes ->
      match WF.decode bytes with
      | Result.Ok _ -> true
      | Result.Error _ -> false)

(* Property: Small valid varints decode successfully *)
let small_varint_prop =
  property "WireFormat decode handles single-byte varints"
    Arbitrary.(make (Generator.int_range 0 127))
    (fun n ->
      (* Single byte varint: field 1, value n *)
      (* Tag: (1 << 3) | 0 = 8, then value n *)
      let bytes = IO.Bytes.of_string (String.make 1 (Char.chr 8) ^ String.make 1 (Char.chr n)) in
      match WF.decode bytes with
      | Result.Ok _ -> true
      | Result.Error _ -> false)

(* Property: Round-trip encode/decode preserves data
   
   This test uses the actual encode function to generate valid wire format,
   then verifies decode(encode(x)) produces data that re-encodes identically.
*)
let roundtrip_prop =
  property "WireFormat encode/decode round-trip preserves data"
    Arbitrary.(make bytes_gen)
    (fun original_bytes ->
      (* First, try to decode the bytes *)
      match WF.decode original_bytes with
      | Result.Error _ -> 
          (* If original bytes don't decode, skip this example *)
          true
      | Result.Ok decoded ->
          (* If decode succeeds, encode and then decode again *)
          let encoded = WF.encode decoded in
          match WF.decode encoded with
          | Result.Error _ -> 
              (* Re-encoded bytes should always decode successfully *)
              fail "Re-encoding produced invalid bytes"
          | Result.Ok redecoded ->
              (* Re-encode and verify stability: encode(decode(encode(x))) = encode(x) *)
              let reencoded = WF.encode redecoded in
              IO.Bytes.equal encoded reencoded)

let tests = [
  decode_never_crashes_prop;
  empty_bytes_decode_prop;
  small_varint_prop;
  roundtrip_prop;
]

let () =
  Miniriot.run 
    ~main:(fun ~args:_ -> Test.Cli.main ~name:"protobuf:wire_properties" ~tests ~args:Env.args) 
    ~args:Env.args ()
