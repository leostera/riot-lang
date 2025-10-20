# Protobuf Package

OCaml implementation of Protocol Buffers for the Riot framework.

## Overview

This package provides three main modules for working with Protocol Buffers:

1. **ProtofileFormat**: Parser and printer for `.proto` definition files
2. **DebugFormat**: Parser and printer for protobuf text/debug format
3. **WireFormat**: Encoder and decoder for the binary wire format

## Architecture

### ProtofileFormat

Parses `.proto` files into an AST representation.

```ocaml
open Protobuf

let proto_source = {|
  syntax = "proto3";
  
  message Person {
    string name = 1;
    int32 age = 2;
  }
|}

match ProtofileFormat.parse proto_source with
| Ok ast -> 
    let output = ProtofileFormat.print ast in
    print_endline output
| Error err -> 
    Printf.eprintf "Parse error: %s\n" err
```

**Features:**
- Full proto3 and Edition 2024 syntax support
- Messages, enums, services, options
- Nested definitions
- Maps, oneofs, reserved fields
- Import statements and packages

### DebugFormat

Parses and prints Protocol Buffer text format (debug format).

```ocaml
open Protobuf

let text = {|
  name: "John Doe"
  age: 30
  address {
    street: "123 Main St"
    city: "Seattle"
  }
|}

match DebugFormat.parse text with
| Ok message ->
    let output = DebugFormat.print message in
    print_endline output
| Error err ->
    Printf.eprintf "Parse error: %s\n" err
```

**Features:**
- Field names and values
- Nested messages (both `{}` and `<>` syntax)
- Repeated fields (list syntax)
- Extension fields
- Comments

### WireFormat

Encodes and decodes binary Protocol Buffer messages.

```ocaml
open Protobuf.WireFormat

(* Create a message *)
let message = [
  { field_number = 1; value = Varint (Uint32 150l) };
  { field_number = 2; value = Len (String "hello") };
]

(* Encode to bytes *)
let encoded = encode message

(* Decode from bytes *)
match decode encoded with
| Ok decoded_message ->
    Printf.printf "Decoded %d fields\n" (List.length decoded_message)
| Error err ->
    Printf.eprintf "Decode error: %s\n" err
```

**Wire Types:**
- `WtVarint`: Variable-length integers
- `WtI64`: 64-bit fixed-width
- `WtI32`: 32-bit fixed-width
- `WtLen`: Length-delimited (strings, bytes, messages)
- `WtSgroup`/`WtEgroup`: Groups (deprecated)

**Value Types:**
- Varint values: `Int32`, `Int64`, `Uint32`, `Uint64`, `Sint32`, `Sint64`, `Bool`, `Enum`
- Fixed64 values: `Fixed64`, `Sfixed64`, `Double`
- Fixed32 values: `Fixed32`, `Sfixed32`, `Float`
- Length-delimited: `String`, `Bytes`, `Message`, `PackedVarint`, `PackedI32`, `PackedI64`
- Groups: Deprecated wire format feature

## Implementation Details

### Recursive Descent Parsing

All parsers use `Std.Iter.MutCursor` for efficient single-pass parsing:

```ocaml
open Std.Iter.MutCursor

let parse_field cursor =
  skip_whitespace cursor;
  match parse_ident cursor with
  | Error e -> Error e
  | Ok name ->
      skip_whitespace cursor;
      (* ... more parsing ... *)
```

### Wire Format Encoding

The wire format uses varints and field tags:

```
tag = (field_number << 3) | wire_type
```

Example encoding:
```
Field 1, value 150:
  Tag: 0x08 = (1 << 3) | 0 = 0b00001000
  Value: 150 = 0x96 0x01 (varint encoding)
  Full: 08 96 01
```

### ZigZag Encoding

Signed integers use ZigZag encoding for efficient negative number representation:

```ocaml
let encode_zigzag n =
  Int64.logxor (Int64.shift_left n 1) (Int64.shift_right n 63)

(* Examples:
    0 -> 0
   -1 -> 1
    1 -> 2
   -2 -> 3
*)
```

## Testing

The package includes a comprehensive test suite:

```bash
cd packages/protobuf/tests

# Run all tests
python3 test_runner.py all

# Run specific format tests
python3 test_runner.py wire
python3 test_runner.py debug
python3 test_runner.py protofile

# Run Google's conformance tests
python3 test_runner.py conformance
```

### Test Fixtures

Located in `tests/fixtures/`:

- **wire/**: Binary wire format test cases
- **debug/**: Text format test cases
- **protofile/**: Proto definition parsing tests

### Conformance Tests

Google's official conformance test suite is available at:
`../../3rdparty/protobuf/conformance/`

These tests provide comprehensive coverage of:
- Binary wire format encoding/decoding
- JSON format (future)
- Text format parsing
- Edge cases and error handling

## References

### Specifications

Grammars and specifications are in `src/`:

- `proto3_2024_protofile.ebnf`: EBNF grammar for .proto files
- `proto3_2024_debug.ebnf`: EBNF grammar for text format
- `WIRE_FORMAT.md`: Wire format encoding specification

### External Documentation

- [Protocol Buffers Edition 2024 Spec](https://protobuf.dev/reference/protobuf/edition-2024-spec/)
- [Text Format Spec](https://protobuf.dev/reference/protobuf/textformat-spec/)
- [Wire Format Encoding](https://protobuf.dev/programming-guides/encoding/)
- [Proto3 Language Guide](https://protobuf.dev/programming-guides/proto3/)

## Current Status

✅ **Implemented:**
- Wire format encoding/decoding (basic types)
- Debug format parsing (basic messages and fields)
- Protofile lexer and parser foundations
- Test infrastructure

🚧 **In Progress:**
- Full protofile parser implementation
- Conformance test integration
- Advanced wire format features (groups, packed fields)

📋 **Planned:**
- Code generation from .proto files
- JSON format support
- Schema validation
- Descriptor APIs

## Dependencies

- `std`: Riot standard library
- Uses `Std.Iter.MutCursor` for parsing
- Uses `Std.Cell` for mutable state in parsers

## License

See LICENSE file in repository root.
