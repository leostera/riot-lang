# Protocol Buffers Wire Format Specification

Source: https://protobuf.dev/programming-guides/encoding/

## Overview

The protocol buffer wire format is a binary encoding that defines how messages are serialized for transmission or storage. It's designed to be:
- **Compact**: Small values use fewer bytes
- **Forward/backward compatible**: Old parsers can skip unknown fields
- **Self-describing**: Each field includes its type information

## Core Concepts

### 1. Base 128 Varints

Variable-width integers are the foundation of the wire format. They encode unsigned 64-bit integers using 1-10 bytes.

**Encoding Rules:**
- Each byte has a continuation bit (MSB) indicating if more bytes follow
- Lower 7 bits of each byte form the payload
- Payload bytes are in little-endian order

**Examples:**
```
Number 1:
  0000 0001
  ^ MSB not set (single byte)

Number 150:
  10010110 00000001
  ^ MSB    ^ MSB
  
  Decoding:
  1. Drop MSBs: 0010110 0000001
  2. Reverse to big-endian: 0000001 0010110
  3. Concatenate: 00000010010110
  4. Result: 128 + 16 + 4 + 2 = 150
```

### 2. Wire Types

There are 6 wire types that determine how to parse the payload:

| ID | Name   | Used For                                              |
|----|--------|-------------------------------------------------------|
| 0  | VARINT | int32, int64, uint32, uint64, sint32, sint64, bool, enum |
| 1  | I64    | fixed64, sfixed64, double                             |
| 2  | LEN    | string, bytes, embedded messages, packed repeated     |
| 3  | SGROUP | group start (deprecated)                              |
| 4  | EGROUP | group end (deprecated)                                |
| 5  | I32    | fixed32, sfixed32, float                              |

### 3. Message Structure

A message is a series of **records**, where each record is:
```
tag + payload
```

**Tag Format:**
```
tag = (field_number << 3) | wire_type
```

The tag is encoded as a varint. The low 3 bits specify the wire type, the remaining bits specify the field number.

**Example:**
```
Field number: 1, Wire type: VARINT (0)
Tag calculation: (1 << 3) | 0 = 8 = 0x08

Binary: 0000 1000
        ^^^^^ ^^^
        field wire_type
```

## Field Type Encodings

### VARINT Types (wire type 0)

#### Unsigned Integers (int32, int64, uint32, uint64)
- Encoded directly as varint
- Negative numbers waste space (use all 10 bytes for int32/int64)

#### Bools
- `false` = 0x00
- `true` = 0x01

#### Enums
- Encoded as int32

#### Signed Integers (sint32, sint64)
- Use ZigZag encoding to efficiently handle negative numbers
- Formula: `(n << 1) ^ (n >> 31)` for sint32
- Formula: `(n << 1) ^ (n >> 63)` for sint64

**ZigZag Mapping:**
| Signed | Encoded |
|--------|---------|
| 0      | 0       |
| -1     | 1       |
| 1      | 2       |
| -2     | 3       |
| 2      | 4       |
| ...    | ...     |

### Fixed-Width Types

#### I64 (wire type 1)
- `fixed64`: 8 bytes, little-endian unsigned integer
- `sfixed64`: 8 bytes, little-endian signed integer
- `double`: 8 bytes, IEEE 754 double-precision

#### I32 (wire type 5)
- `fixed32`: 4 bytes, little-endian unsigned integer
- `sfixed32`: 4 bytes, little-endian signed integer
- `float`: 4 bytes, IEEE 754 single-precision

### Length-Delimited Types (wire type 2)

Format: `tag + length_varint + payload`

#### Strings
- Length prefix (varint encoding the byte count)
- UTF-8 encoded bytes
- Max size: 2GB

**Example:**
```
Field 2, value "testing":
  Tag: 0x12 (field 2, wire type LEN)
  Length: 0x07 (7 bytes)
  Payload: 0x74 65 73 74 69 6E 67 ("testing" in ASCII)
  
Full encoding: 12 07 74 65 73 74 69 6E 67
```

#### Bytes
- Same as strings but can contain arbitrary bytes (not necessarily UTF-8)

#### Embedded Messages (Submessages)
- Length prefix
- Serialized message bytes

**Example:**
```
message Test1 { int32 a = 1; }
message Test3 { Test1 c = 3; }

Test3 with c.a = 150:
  Tag for field c: 0x1A (field 3, wire type LEN)
  Length: 0x03 (3 bytes)
  Submessage: 08 96 01 (encoding of Test1{a:150})
  
Full encoding: 1A 03 08 96 01
```

#### Packed Repeated Fields
- Default for repeated primitive types (Edition 2023+)
- Single LEN record containing all values concatenated
- No tags between elements

**Example:**
```
repeated int32 e = 6; // values: [3, 270, 86942]

Tag: field 6, wire type LEN
Length: depends on concatenated varint sizes
Payload: 03 8E 02 9E A7 05 (varints concatenated)
```

**Unpacked Repeated Fields:**
- One record per value
- Used for strings, bytes, messages
- Order preserved

```
repeated int32 e = 6; // values: [1, 2, 3]

Encoding: 
  30 01  (field 6, value 1)
  30 02  (field 6, value 2)
  30 03  (field 6, value 3)
```

## Special Cases

### Maps
Maps are syntactic sugar for repeated messages:

```
map<string, int32> g = 7;

// Equivalent to:
message g_Entry {
  string key = 1;
  int32 value = 2;
}
repeated g_Entry g = 7;
```

Encoded as a sequence of LEN records, each containing key and value fields.

### Oneofs
- Encoded the same as regular fields
- Parser responsibility to enforce "only one set" semantics

### Groups (Deprecated)
- Use SGROUP/EGROUP wire types
- SGROUP tag marks start, EGROUP tag marks end
- Both have empty payloads
- Field numbers must match

## Parsing Rules

### Missing Fields
- Fields not present in the wire format are absent from the message
- Sparse encoding: only set fields are encoded

### Unknown Fields
- Fields with unrecognized numbers should be preserved
- Allows forward compatibility

### Duplicate Fields
- **Non-repeated fields**: Last value wins
- **Embedded messages**: Merged (via MergeFrom semantics)
- **Repeated fields**: Concatenated in order

### Message Concatenation
```
ParseFromString(str1 + str2) ≡ Parse(str1).MergeFrom(Parse(str2))
```

## Limits

- Maximum message size: **2GB** (2^31 bytes)
- Maximum string/bytes field size: **2GB**
- Field numbers: 1 to 536,870,911 (excluding 19,000-19,999 reserved range)

## Condensed Reference

```
message    := (tag value)*
tag        := (field << 3) | wire_type     // encoded as varint
value      := varint      for VARINT
            | i32         for I32
            | i64         for I64
            | len-prefix  for LEN
            | <empty>     for SGROUP/EGROUP

varint     := variable-length encoding of integers
              (sint types use ZigZag encoding first)
              
i32        := 4-byte little-endian
i64        := 8-byte little-endian

len-prefix := size (message | string | bytes | packed)
              size encoded as varint
```

## Implementation Notes

### Non-deterministic Serialization
- Field order is not guaranteed
- Repeated serialization of the same message may produce different byte outputs
- Do not rely on byte-level equality for message comparison
- Hashing serialized messages is unreliable

### Compatibility Guidelines
- Field numbers are forever (never reuse)
- Wire types can change if compatible (e.g., int32 ↔ int64)
- Packed ↔ unpacked repeated fields are compatible
- Parser must handle both packed and unpacked for repeated fields
