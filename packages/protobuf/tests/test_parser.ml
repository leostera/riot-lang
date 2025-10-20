open Std

let test_simple_message () =
  let proto =
    {|
syntax = "proto3";

package example;

message Person {
  string name = 1;
  int32 age = 2;
  string email = 3;
}
|}
  in
  match Protobuf.ProtofileFormat.parse proto with
  | Ok ast ->
      Printf.printf "✓ Parsed simple message successfully\n";
      Printf.printf "  Package: %s\n"
        (match ast.package with Some p -> p | None -> "(none)");
      Printf.printf "  Definitions: %d\n" (List.length ast.definitions);
      true
  | Error err ->
      Printf.eprintf "✗ Parse error: %s\n" err;
      false

let test_enum_def () =
  let proto =
    {|
syntax = "proto3";

enum Status {
  STATUS_UNSPECIFIED = 0;
  STATUS_ACTIVE = 1;
  STATUS_INACTIVE = 2;
}
|}
  in
  match Protobuf.ProtofileFormat.parse proto with
  | Ok ast ->
      Printf.printf "✓ Parsed enum successfully\n";
      Printf.printf "  Definitions: %d\n" (List.length ast.definitions);
      true
  | Error err ->
      Printf.eprintf "✗ Parse error: %s\n" err;
      false

let test_service () =
  let proto =
    {|
syntax = "proto3";

service SearchService {
  rpc Search(SearchRequest) returns (SearchResponse);
}
|}
  in
  match Protobuf.ProtofileFormat.parse proto with
  | Ok ast ->
      Printf.printf "✓ Parsed service successfully\n";
      Printf.printf "  Definitions: %d\n" (List.length ast.definitions);
      true
  | Error err ->
      Printf.eprintf "✗ Parse error: %s\n" err;
      false

let test_debug_format () =
  let text = {|
name: "John Doe"
age: 30
email: "john@example.com"
|} in
  match Protobuf.DebugFormat.parse text with
  | Ok fields ->
      Printf.printf "✓ Parsed debug format successfully\n";
      Printf.printf "  Fields: %d\n" (List.length fields);
      true
  | Error err ->
      Printf.eprintf "✗ Parse error: %s\n" err;
      false

let test_wire_format () =
  let open Protobuf.WireFormat in
  let message = [ { field_number = 1; value = Varint (Uint64 150L) } ] in
  let encoded = encode message in
  Printf.printf "✓ Encoded message: %d bytes\n" (Stdlib.Bytes.length encoded);
  match decode encoded with
  | Ok decoded ->
      Printf.printf "✓ Decoded message successfully\n";
      Printf.printf "  Records: %d\n" (List.length decoded);
      true
  | Error err ->
      Printf.eprintf "✗ Decode error: %s\n" err;
      false

let () =
  Printf.printf "\n=== Running Protobuf Tests ===\n\n";
  let tests =
    [
      ("Simple Message", test_simple_message);
      ("Enum Definition", test_enum_def);
      ("Service Definition", test_service);
      ("Debug Format", test_debug_format);
      ("Wire Format", test_wire_format);
    ]
  in
  let results =
    List.map
      (fun (name, test) ->
        Printf.printf "Testing %s...\n" name;
        let result = test () in
        Printf.printf "\n";
        result)
      tests
  in
  let passed =
    List.fold_left (fun acc r -> if r then acc + 1 else acc) 0 results
  in
  let total = List.length results in
  Printf.printf "=== Summary ===\n";
  Printf.printf "%d/%d tests passed\n" passed total;
  if passed = total then exit 0 else exit 1
