# Bytecode Loading Implementation Guide

## Current Status

We've implemented a **basic bytecode loader skeleton** with:
- ✅ File type detection (executable, .cmo, .cma)
- ✅ Magic number validation
- ✅ Trailer parsing for executables
- ✅ Basic instruction reading
- ✅ Primitive name parsing
- ⏳ Partial .cmo support (needs marshaling parser)
- ⏳ No .cma support yet

## What Works Now

```rust
// Can detect file type
let bytecode = BytecodeLoader::load("program.out")?;

// Can read executables with trailer
// Can parse primitive names
// Can read bytecode instructions
```

## What's Missing: The Marshaling Format

The **critical missing piece** is OCaml's marshaling format parser.

### Why Marshaling Matters

OCaml uses a binary serialization format called "marshaling" to encode:
- Constants (strings, floats, ints)
- Data structures (lists, arrays, records)
- Closures
- Compilation unit metadata

**In .cmo files**: The header is a marshaled `compilation_unit` structure  
**In executables**: The DATA section contains marshaled global values

### OCaml Marshaling Format

```text
┌─────────────────────────────────────────┐
│  MARSHALED VALUE FORMAT                 │
├─────────────────────────────────────────┤
│  Magic: 0x8495A6BE (intext magic)      │
│  Block length (4 bytes)                 │
│  Num objects (4 bytes)                  │
│  Size 32-bit (4 bytes)                  │
│  Size 64-bit (4 bytes)                  │
├─────────────────────────────────────────┤
│  OBJECTS:                               │
│  ┌───────────────────────────────────┐  │
│  │ Tag byte determines type:         │  │
│  │   0x00-0x7F: Small int (value)    │  │
│  │   0x80: Header (followed by data) │  │
│  │   0x81-0x8F: Block tags           │  │
│  │   0x90: String                     │  │
│  │   0x91: Float                      │  │
│  │   0xFC: Int32                      │  │
│  │   0xFD: Int64                      │  │
│  │   0xFE: Shared reference          │  │
│  └───────────────────────────────────┘  │
└─────────────────────────────────────────┘
```

### Example: Marshaled Integer

```rust
// Marshal format for integer 42:
// [0x8495A6BE] [len] [objs] [size32] [size64] [TAG] [VALUE]
//   magic       ?     ?      ?        ?         0x54  (42<<1|1)

// TAG 0x54 = small integer in 7-bit encoding
// VALUE = 42 << 1 | 1 = 85 = 0x55
```

### Example: Marshaled String

```rust
// Marshal format for string "hello":
// [magic] [header] [0x90] [5] ['h']['e']['l']['l']['o']
//                   ^^^^   ^^^  ^^^^^^^^^^^^^^^^^^^^^^
//                   string len   characters
//                   tag

// 0x90 = String tag
// 5 = length
// followed by bytes
```

### Example: Marshaled Block

```rust
// Marshal format for (1, "test"):
// [magic] [header] [0x80] [tag] [size] [field1] [field2]
//                   ^^^^   ^^^^  ^^^^
//                   block  tuple 2
//                   header

// 0x80 = Block header
// tag = 0 (tuple)
// size = 2 (two fields)
// field1 = marshaled 1
// field2 = marshaled "test"
```

## Implementation Plan

### Phase 1: Basic Marshaling (1-2 days) ⚠️ HIGH PRIORITY

Implement support for:
- ✅ Small integers (TAG 0x00-0x7F)
- ✅ Strings (TAG 0x90)
- ✅ Blocks (TAG 0x80)
- ✅ Int32 (TAG 0xFC)
- ✅ Int64 (TAG 0xFD)
- ✅ Float (TAG 0x91)

**File**: Create `raml/src/runtime/marshal.rs`

**API**:
```rust
pub struct MarshalReader {
    data: Vec<u8>,
    pos: usize,
}

impl MarshalReader {
    pub fn new(data: Vec<u8>) -> Self;
    pub fn read_value(&mut self) -> Result<Value>;
    
    fn read_header(&mut self) -> Result<MarshalHeader>;
    fn read_object(&mut self) -> Result<Value>;
    fn read_string(&mut self) -> Result<String>;
    fn read_block(&mut self) -> Result<*mut Block>;
}
```

### Phase 2: Compilation Unit Parser (1 day)

Parse the .cmo header structure:

```ocaml
type compilation_unit = {
  cu_name: string;            (* Module name *)
  cu_pos: int;                (* Bytecode position *)
  cu_codesize: int;           (* Code size in bytes *)
  cu_reloc: reloc_info list;  (* Relocations *)
  cu_imports: import list;    (* Imports *)
  cu_primitives: string list; (* Primitive names *)
  cu_force_link: bool;        (* Force linking? *)
  cu_debug: int;              (* Debug offset *)
  cu_debugsize: int;          (* Debug size *)
}
```

**Update**: `bytecode.rs::read_compilation_unit()`

### Phase 3: DATA Section Parser (1 day)

Parse marshaled global data:

```rust
fn read_data_section(file: &mut File, trailer: &ExecTrailer) -> Result<Vec<Value>> {
    // Read section
    let data = read_section_bytes(file, trailer.sections[1])?;
    
    // Unmarshal values
    let mut reader = MarshalReader::new(data);
    let mut values = Vec::new();
    
    while !reader.at_end() {
        values.push(reader.read_value()?);
    }
    
    Ok(values)
}
```

### Phase 4: Archive Support (2 days) - OPTIONAL

Parse .cma archive files:

```ocaml
type library = {
  lib_units: compilation_unit list;  (* List of .cmo units *)
  lib_custom: bool;                  (* Custom runtime? *)
  lib_ccobjs: string list;           (* C objects *)
  lib_ccopts: string list;           (* C compiler options *)
  lib_dllibs: string list;           (* DLL libraries *)
}
```

**Update**: `bytecode.rs::load_cma()`

### Phase 5: Testing (1 day)

Create test cases:

```rust
#[test]
fn test_load_simple_cmo() {
    // Compile: ocamlc -c test.ml
    let bytecode = BytecodeLoader::load("test.cmo").unwrap();
    assert!(!bytecode.code.is_empty());
}

#[test]
fn test_marshal_integer() {
    let data = vec![0x84, 0x95, 0xA6, 0xBE, /* header */, 0x54];
    let mut reader = MarshalReader::new(data);
    let value = reader.read_value().unwrap();
    assert_eq!(value.as_int(), 42);
}

#[test]
fn test_marshal_string() {
    let data = vec![0x84, 0x95, 0xA6, 0xBE, /* header */, 0x90, 0x05, 
                    b'h', b'e', b'l', b'l', b'o'];
    let mut reader = MarshalReader::new(data);
    let value = reader.read_value().unwrap();
    // Assert it's a string block
}
```

## Reference Implementation

The OCaml runtime has the reference implementation:
- **File**: `ocaml/runtime/intern.c` (unmarshaling)
- **File**: `ocaml/runtime/extern.c` (marshaling)

Key functions:
- `caml_input_value()` - Read marshaled value
- `intern_rec()` - Recursive unmarshaling
- `readblock()` - Read block header

## Example Real Bytecode

Let's examine our test file:

```bash
$ hexdump -C test.cmo | head -20
00000000  43 61 6d 6c 31 39 39 39  4f 30 33 35 00 00 29 65  |Caml1999O035..)e|
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^
          Magic: "Caml1999O035"        Start of marshaled data

00000010  54 00 00 00 6d 02 00 00  29 00 00 00 2a 00 00 00  |T...m...)...*...|
          ^^                        ^^^^^^^^^^  ^^^^^^^^^^
          Tag?                      Values      Values

00000020  02 00 00 00 02 00 00 00  0b 00 00 00 0d 00 00 00  |................|
          Marshaled compilation unit structure continues...
```

## Quick Win: Hand-Crafted Bytecode

While implementing the marshaling parser, we can test the interpreter with **hand-crafted bytecode**:

```rust
// Create bytecode manually (no file loading needed)
let bytecode = LoadedBytecode {
    code: vec![
        Opcode::ConstantInt, 42,       // Load 42
        Opcode::C_Call1, 0,            // Call print_int
        Opcode::Stop,                  // Stop
    ],
    data: vec![],
    primitives: vec!["print_int".to_string()],
    symbols: vec![],
};

let mut runtime = Runtime::new();
runtime.load_bytecode_direct(bytecode);
runtime.run()?;  // Prints: 42
```

This lets us **test the runtime immediately** without waiting for the marshaling parser!

## Summary

**What we have**: Basic structure, file detection, instruction reading  
**What we need**: Marshaling format parser (1-2 weeks)  
**Quick alternative**: Hand-craft bytecode for testing (works today!)

**Critical path**:
1. Implement basic marshaling (integers, strings, blocks) - 2 days
2. Parse .cmo compilation unit header - 1 day  
3. Test with real .cmo files - 1 day

**Total**: ~4 days of focused work to load real .cmo files! 🚀

## Resources

- OCaml Internals: https://ocaml.org/manual/intf-c.html
- Marshal format: `ocaml/runtime/caml/intext.h`
- Bytecode format: `ocaml/bytecomp/bytecode.mli`
- Example unmarshaler: `ocaml/runtime/intern.c`

---

**Next Steps**: Start with `marshal.rs` - implement integer and string unmarshaling first!
