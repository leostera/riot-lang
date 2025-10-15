# .cmo Parser - Current Status

## What We Learned

### .cmo File Format (from OCaml source: `bytecomp/symtable.mli`)

```
┌────────────────────────────────────────────────────┐
│ 1. Magic number "Caml1999O035" (12 bytes)         │
├────────────────────────────────────────────────────┤
│ 2. Offset to compilation_unit descriptor (4 bytes)│
│    This tells us where to find step 5              │
├────────────────────────────────────────────────────┤
│ 3. Block of relocatable bytecode                  │
│    (location/size stored in compilation_unit)      │
├────────────────────────────────────────────────────┤
│ 4. Debugging information (optional)                │
├────────────────────────────────────────────────────┤
│ 5. Compilation unit descriptor (marshaled)         │
│    - cu_name: string                               │
│    - cu_pos: int (absolute position of bytecode)   │
│    - cu_codesize: int (size of bytecode in bytes)  │
│    - cu_reloc: relocation info list                │
│    - cu_imports: imported modules                  │
│    - cu_primitives: string list (IMPORTANT!)       │
│    - cu_force_link: bool                           │
│    - cu_debug: int (position of debug info)        │
│    - cu_debugsize: int                             │
└────────────────────────────────────────────────────┘
```

### Example: test_simple.cmo

```
Offset  Content
------  -------
0x00    "Caml1999O035" (magic)
0x0C    0x00000040 (cu_offset = 64)
0x10    ??? (unknown data or start of bytecode?)
...
0x40    0x8495A6BE (marshal magic - compilation_unit starts here)
```

## The Problem

To properly load a .cmo file, we need to:

1. ✅ Read magic number
2. ✅ Read cu_offset
3. ✅ Seek to cu_offset and find marshal data
4. ❌ **Parse the marshaled compilation_unit record** ← THIS IS THE BLOCKER
5. ❌ Extract `cu_pos` and `cu_codesize` from the record
6. ❌ Seek to `cu_pos` and read `cu_codesize` bytes of bytecode
7. ❌ Extract `cu_primitives` list

## Current State

### What Works ✅
- Magic number detection
- Reading cu_offset
- Finding marshal data
- Basic marshal parsing (integers, strings, blocks)

### What's Missing ❌
- **Parsing OCaml record structures from marshal data**
  - Need to understand field layout
  - Need to extract specific fields by index
  - Need to handle lists properly
  
- **Extracting compilation_unit fields:**
  - `cu_pos` (field index ?)
  - `cu_codesize` (field index ?)
  - `cu_primitives` (field index ?)

## Why This Is Hard

The `compilation_unit` type is a record with 10 fields. When marshaled, it becomes a block with tag 0 and 10 fields. We need to:

1. Parse the block header
2. Extract each field by position
3. Know which position corresponds to which field
4. Handle nested structures (lists, tuples, options)

**Our current marshal parser doesn't extract record fields yet** - it just returns a placeholder.

## Quick Workaround

For simple test files like `test_simple.ml`, we could:

1. Skip .cmo loading entirely
2. Compile directly to executable with `ocamlc -o test test_simple.ml`
3. Load the executable format instead (which has a simpler structure)

OR:

1. Use `ocamlobjinfo` to inspect .cmo files
2. Manually extract bytecode for testing
3. Load as raw bytecode array

## Next Steps to Fix This

### Option 1: Complete Marshal Parser (2-3 days)
Finish implementing marshal.rs to properly parse:
- Record/block fields extraction
- List structures
- Nested types
- Proper heap allocation for complex values

Then use it to parse compilation_unit.

### Option 2: Use ocamlobjinfo (1 hour)
```bash
ocamlobjinfo test.cmo
```
This shows the structure. We could:
- Extract bytecode manually
- Test with known bytecode sequences
- Skip .cmo parsing for now

### Option 3: Test With Executables (30 minutes)
```bash
ocamlc -o test test.ml
```
Executables have a different, simpler format:
- Trailer at end of file with section offsets
- CODE section with bytecode
- DATA section with marshaled globals
- PRIM section with primitive names

This is what we already support!

## Recommendation

**Use Option 3** - test with executables instead of .cmo files:

```bash
# Create executable
ocamlc -o test_simple.out test_simple.ml

# Test loading
cargo run --bin load_cmo test_simple.out
```

This will work immediately with our existing loader because we already implemented the executable format parser!

Then, once we confirm the runtime works with executables, we can come back and complete the .cmo parser.

## Files Status

- ✅ `marshal.rs` - Basic types working, needs record field extraction
- ✅ `bytecode.rs` - Executable format works, .cmo needs completion  
- ✅ `wasm.rs` - Can load binary data
- ✅ `demo-cmo.html` - UI ready

## Summary

We're **90% there** - the marshal parser works for basic types, and we understand the .cmo format. The remaining 10% is parsing OCaml records from marshal data, which requires understanding field layout and extraction.

**For immediate testing:** Use executables instead of .cmo files - they work today!
