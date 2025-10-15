# OCaml Marshal Format Investigation

**Date:** October 12, 2025  
**Goal:** Parse .cmo files to extract bytecode and primitives list  
**Status:** 90% complete - can parse block headers, need string encoding

## Summary

We successfully reverse-engineered most of OCaml's marshal format by reading the C runtime source code and testing with real .cmo files.

### What Works ✅

1. **Marshal header parsing** (20 bytes)
2. **CODE_BLOCK32 recognition** (0x08)
3. **Block header decoding** (size + tag from 32-bit header)
4. **Finding compilation_unit record** (Block with tag 0, 10 fields)

### What's Remaining ❌

**String encoding inside blocks** - We can read the block structure but strings inside aren't parsing correctly.

## OCaml Marshal Format

### Header Format (20 bytes - "small header")

```
Offset | Size | Field
-------|------|------------------
0      | 4    | Magic: 0x8495A6BE
4      | 4    | Block length (bytes)
8      | 4    | Number of objects
12     | 4    | Size for 32-bit
16     | 4    | Size for 64-bit
```

There are actually THREE header formats:
- **Small** (20 bytes) - for normal objects
- **Big** (32 bytes) - for very large objects (64-bit)  
- **Compressed** (variable) - for compressed marshaling

### Object Codes

From `ocaml/compiler/runtime/caml/intext.h`:

| Code | Hex  | Meaning |
|------|------|---------|
| CODE_INT8 | 0x00 | 8-bit integer follows |
| CODE_INT16 | 0x01 | 16-bit integer follows |
| CODE_INT32 | 0x02 | 32-bit integer follows |
| CODE_INT64 | 0x03 | 64-bit integer follows |
| CODE_SHARED8 | 0x04 | 8-bit offset to shared object |
| CODE_SHARED16 | 0x05 | 16-bit offset to shared object |
| CODE_SHARED32 | 0x06 | 32-bit offset to shared object |
| **CODE_BLOCK32** | **0x08** | **Block with 32-bit header** |
| CODE_STRING8 | 0x09 | String with 8-bit length |
| CODE_STRING32 | 0x0A | String with 32-bit length |
| CODE_DOUBLE | 0x0B | 64-bit float |
| CODE_DOUBLE_ARRAY8 | 0x0D | Float array, 8-bit length |
| CODE_BLOCK64 | 0x13 | Block with 64-bit header |
| CODE_SHARED64 | 0x14 | 64-bit offset to shared object |
| **PREFIX_SMALL_INT** | **0x40-0x7F** | **Small int (value = code - 0x40)** |
| **PREFIX_SMALL_BLOCK** | **0x80-0xFF** | **Small block (tag = code & 0xF, size = (code >> 4) & 0x7)** |

### Block Header Format

After CODE_BLOCK32 (0x08), read 32-bit header:

```rust
let header = read_u32_be();
let size = header >> 10;   // Number of fields
let tag = header & 0xFF;    // Block tag
```

Then read `size` objects recursively.

## .cmo File Format

From `ocaml/compiler/file_formats/cmo_format.mli`:

```
┌─────────────────────────────────────────┐
│  Magic: "Caml1999O035" (12 bytes)       │
├─────────────────────────────────────────┤
│  Offset to compilation_unit (4 bytes)   │  ← Read with input_binary_int
├─────────────────────────────────────────┤
│  Bytecode instructions                  │
├─────────────────────────────────────────┤
│  Debug info (optional)                  │
├─────────────────────────────────────────┤
│  Compilation unit (marshaled)           │  ← Seek to offset, then input_value
└─────────────────────────────────────────┘
```

### compilation_unit Record

```ocaml
type compilation_unit = {
  cu_name: compunit;                   (* Field 0 - string (unboxed) *)
  cu_pos: int;                        (* Field 1 *)
  cu_codesize: int;                   (* Field 2 *)
  cu_reloc: (reloc_info * int) list;  (* Field 3 *)
  cu_imports: crcs;                   (* Field 4 *)
  cu_required_compunits: compunit list; (* Field 5 *)
  cu_primitives: string list;         (* Field 6 ← WE NEED THIS! *)
  cu_force_link: bool;                (* Field 7 *)
  cu_debug: int;                      (* Field 8 *)
  cu_debugsize: int;                  (* Field 9 *)
}
```

**Note:** `compunit = Compunit of string [@@unboxed]` means it's marshaled as just a string.

## Test Case

File: `./ocaml/ocamlformat/_build/default/lib/.ocamlformat_lib.objs/byte/ocamlformat_lib__Chunk.cmo`

### Observed Bytes at Compilation Unit (offset 10597)

```
Hex Offset | Bytes                                    | Interpretation
-----------|------------------------------------------|-----------------
10597      | 84 95 a6 be                              | Marshal magic ✓
10601      | 00 00 1c d8                              | Block length ✓
10605      | 00 00 03 e4                              | Num objects ✓
10609      | 00 00 0f 3d                              | Size 32 ✓
10613      | 00 00 0c 23                              | Size 64 ✓
10617      | 08                                       | CODE_BLOCK32 ✓
10618      | 00 00 2b 00                              | Header: size=10, tag=0 ✓
10622      | 00                                       | Field 0: CODE_INT8?
10623      | 36                                       | = 54?
10624      | 4f 63 61 6d 6c 66 6f 72 6d 61 74 ...    | "Ocamlformat_lib__Chunk" (string data?)
```

### The Mystery

At offset 10622, we see `00 36` followed by ASCII "Ocamlformat...".

**Expected:** CODE_STRING8 (0x09) + length + data  
**Actual:** CODE_INT8 (0x00) + `0x36` + ASCII data

**Hypothesis 1:** The structure changed between OCaml versions?  
**Hypothesis 2:** Strings in blocks have different encoding?  
**Hypothesis 3:** The first field is NOT cu_name but something else?

## Our Implementation

Location: `raml/src/runtime/marshal.rs`

### What We Implemented ✅

```rust
// Correct codes
const CODE_INT8: u8 = 0x00;
const CODE_INT16: u8 = 0x01;
const CODE_INT32: u8 = 0x02;
const CODE_INT64: u8 = 0x03;
const CODE_BLOCK32: u8 = 0x08;
const CODE_STRING8: u8 = 0x09;
// ... etc

// Block parsing
CODE_BLOCK32 => {
    let header = self.read_u32_be()?;
    let size = (header >> 10) as usize;
    let block_tag = (header & 0xFF) as u8;
    self.read_block(block_tag, size)  // Recursively read 'size' fields
}
```

### Test Output

```
✓ CODE_BLOCK32: header=0x2b00, size=10, tag=0
✗ First field: Int(54) instead of String("Ocamlformat_lib__Chunk")
```

We successfully read the block structure but the field contents are wrong.

## Next Steps

### Option A: Debug String Encoding

1. Check if `00 36` means something special (CODE_INT8 + 54 = ???)
2. Look at extern.c to see how strings are written
3. Test with a simpler .cmo file we create ourselves

### Option B: Use Existing Parser

1. Check if there's a Rust crate for OCaml marshal format
2. Or use OCaml's own `input_value` via FFI
3. Or write a minimal OCaml program to parse and dump the structure

### Option C: Heuristic Approach

Since we know:
- Offset 16 to 10597 = bytecode
- Field 6 should be primitives (string list)

We could:
1. Skip parsing the full record
2. Use heuristics to find the primitives list
3. Extract just what we need for the interpreter

## References

- `ocaml/compiler/file_formats/cmo_format.mli` - Format specification
- `ocaml/compiler/runtime/intern.c` - Unmarshaling implementation
- `ocaml/compiler/runtime/extern.c` - Marshaling implementation
- `ocaml/compiler/runtime/caml/intext.h` - Code constants
- `ocaml/compiler/tools/objinfo.ml` - Tool that reads .cmo files

## Key Insights

1. **CODE_BLOCK32 is 0x08, not small int!** - This was the breakthrough
2. **Block headers encode size and tag** - Not separate fields
3. **Records are marshaled as blocks** - Tag 0, N fields
4. **Fields are recursively marshaled** - Each field has its own code
5. **Three header formats exist** - Small (20), Big (32), Compressed (variable)

## Conclusion

We're 90% there! The marshal parser correctly identifies and parses block structures. The remaining 10% is understanding how **strings** (and possibly other types) are encoded when they appear as **fields inside blocks**.

The bytecode IS accessible (offset 16-10597), so worst case we could:
- Extract bytecode directly
- Use a default primitives list
- Test with programs that don't need primitives from the .cmo header

This would get us to **100% bytecode runtime completion** even without perfect .cmo parsing!
