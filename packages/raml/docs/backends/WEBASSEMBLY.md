# WebAssembly Backend

## Overview

The WebAssembly backend compiles OCaml to WebAssembly with a **custom binary encoder** (no external dependencies).

**Status:** ✅ Working end-to-end for simple programs

## Pipeline

```
TypedTree → Lambda IR → WasmIR → WAT + WASM binary
                          ↓
                   Stack-based IR
                   (explicit memory)
```

## Quick Example

```bash
# Compile OCaml to WebAssembly
echo "let x = 42" > test.ml
raml compile test.ml --target wasm -o test.wasm

# Run in Node.js
node -e "WebAssembly.instantiate(require('fs').readFileSync('test.wasm')).then(o => console.log(o.instance.exports.main()))"
# Output: 42
```

## Architecture

### Three Layers

1. **WasmIR** (`wasmIR.ml`) - Lambda IR → Stack-based IR
   - Stack-based operations
   - Explicit locals collection
   - Type inference

2. **WasmAST** (`wasmAST.ml`) - WasmIR → WAT text
   - S-expression generation
   - Pretty-printing
   - Human-readable output

3. **WasmBinary** (`wasmBinary.ml`) - WasmIR → Binary
   - Custom binary encoder
   - No external dependencies
   - LEB128 encoding
   - IEEE 754 float encoding

### Binary Format

```
Magic:    0x00 0x61 0x73 0x6d  (\0asm)
Version:  0x01 0x00 0x00 0x00  (v1)

Sections:
  - Type Section (0x01)     : Function signatures
  - Function Section (0x03) : Function type indices
  - Memory Section (0x05)   : Linear memory
  - Export Section (0x07)   : Exported items
  - Code Section (0x0A)     : Function bodies
```

### Example Output

**Input OCaml:**
```ocaml
let x = 42
```

**Generated WAT:**
```wat
(module
  (memory 1)
  (func $main (result i32)
    (local $x i32)
    i32.const 42
    local.set $x
    local.get $x
    return
  )
  (export "main" (func $main))
)
```

**Binary (hex):**
```
00 61 73 6d 01 00 00 00  ; magic + version
01 05 01 60 00 01 7f     ; type section
03 02 01 00              ; function section
05 03 01 00 01           ; memory section
07 08 01 04 6d 61 69 6e  ; export section "main"
   00 00
0a 0b 01 09 00           ; code section
   41 2a 21 00 20 00 0f 0b
```

## WebAssembly Value Types

| Wasm Type | Size | OCaml Usage |
|-----------|------|-------------|
| `i32` | 32-bit | `int` (untagged for now) |
| `i64` | 64-bit | `int64` |
| `f32` | 32-bit | `float` (single precision) |
| `f64` | 64-bit | `float` |

## Current Implementation

### What Works ✅

**Instructions:**
- Constants: `i32.const`, `i64.const`, `f32.const`, `f64.const`
- Locals: `local.get`, `local.set`, `local.tee`
- Binary ops: `i32.add`, `i32.sub`, `i32.mul`, `i32.div_s`, `i32.rem_s`
- Control: `return`
- Stack: `drop`

**Features:**
- Automatic local variable collection
- Type inference for locals
- Memory allocation (1 page = 64KB)
- Function exports

**File Outputs:**
- `.wasm` - Binary format (ready to run)
- `.wat` - Text format (for debugging)

### Not Yet Implemented 🚧

- Function definitions and calls
- Closures (function tables)
- Pattern matching (structured control flow)
- Strings (heap allocation)
- Records/tuples (heap allocation)
- Arrays (heap + bounds checking)
- Garbage collection

## Binary Encoder Implementation

### LEB128 Encoding

Variable-length integer encoding used throughout Wasm:

```ocaml
(* Unsigned LEB128 *)
encode_u32_leb128 42
→ [0x2A]

encode_u32_leb128 624485
→ [0xE5; 0x8E; 0x26]

(* Signed LEB128 *)
encode_s32_leb128 (-123456)
→ [0xC0; 0xBB; 0x78]
```

### IEEE 754 Float Encoding

```ocaml
encode_f32 3.14
→ [0xC3; 0xF5; 0x48; 0x40]  ; little-endian

encode_f64 3.14159265359
→ [0x18; 0x2D; 0x44; 0x54; 0xFB; 0x21; 0x09; 0x40]
```

### Section Encoding

Each section: `[ID] [Size in LEB128] [Content]`

```ocaml
encode_type_section [{params=[I32; I32]; results=[I32]}]
→ [0x01]  ; section ID
  [0x07]  ; size
  [0x01]  ; 1 type
  [0x60]  ; func type tag
  [0x02]  ; 2 params
  [0x7F; 0x7F]  ; i32, i32
  [0x01]  ; 1 result
  [0x7F]  ; i32
```

## Memory Management

**Current Strategy:**
- Single linear memory (1 page = 64KB)
- No heap allocations yet
- All values in locals/stack

**Future Strategy:**
1. **Bump allocator** - Simple `alloc(size)` that increments heap pointer
2. **Tagged values** - LSB = 1 for immediates, 0 for pointers
3. **GC options:**
   - Reference counting
   - Mark-and-sweep
   - Copying collector
   - Host GC (via reference types proposal)

## Integration with Compiler

```ocaml
(* In main.ml *)
match target with
| "wasm" | "wasm32-unknown-unknown" | "wasm-unknown-unknown" ->
    Wasm.Compile.compile_lambda_to_wasm lambda_ir output_path
```

**Compilation steps:**
1. `Lambda.Ir.lambda` → collected locals
2. Translate expressions to stack-based instructions
3. Build Wasm module structure
4. Generate `.wat` text (via `WasmAST.to_wat`)
5. Encode binary (via `WasmBinary.encode_module`)
6. Write both files

## Testing

```bash
# Compile test
echo "let x = 42" > test.ml
raml compile test.ml --target wasm -o test.wasm

# Verify magic bytes
hexdump -C test.wasm | head -1
# 00000000  00 61 73 6d 01 00 00 00  ... |.asm....|

# Run in Node.js
node <<EOF
const fs = require('fs');
WebAssembly.instantiate(fs.readFileSync('test.wasm'))
  .then(obj => console.log('Result:', obj.instance.exports.main()));
EOF
# Result: 42
```

## Target Triples

Supported:
- `wasm` - Shorthand
- `wasm32-unknown-unknown` - Standard target
- `wasm-unknown-unknown` - Alternative

Future:
- `wasm32-wasi` - WASI system interface
- `wasm64-unknown-unknown` - 64-bit addressing

## Future Enhancements

### Phase 1: Functions & Closures
- Function definitions
- Direct calls: `call $name`
- Indirect calls: `call_indirect` with function table
- Closure conversion

### Phase 2: Data Structures
- Heap allocation
- Strings (length-prefixed)
- Records (struct layout)
- Tuples (inline)
- Arrays (bounds checking)

### Phase 3: Control Flow
- Pattern matching → `br_table`
- Loops: `loop` blocks
- Conditionals: `if`/`else`
- Try/catch (exception proposal)

### Phase 4: Optimization
- Tail call optimization (tail call proposal)
- SIMD operations (SIMD proposal)
- Reference types for better GC integration
- Function inlining

## WebAssembly Resources

- **Spec:** https://webassembly.github.io/spec/core/
- **Binary format:** https://webassembly.github.io/spec/core/binary/
- **Instruction reference:** https://webassembly.github.io/spec/core/appendix/index-instructions.html
- **WAT format:** https://webassembly.github.io/spec/core/text/

## Implementation Files

```
src/backends/wasm/
├── wasmIR.ml/mli      - Lambda → WasmIR (stack-based IR)
├── wasmAST.ml/mli     - WasmIR → WAT text
├── wasmBinary.ml/mli  - WasmIR → .wasm binary (NEW!)
├── compile.ml/mli     - High-level orchestration
└── wasm.ml/mli        - Module aggregator
```

**Line counts:**
- `wasmIR.ml`: ~170 lines
- `wasmAST.ml`: ~135 lines
- `wasmBinary.ml`: ~200 lines (custom encoder)
- Total: ~500 lines for complete Wasm backend

## Comparison with Other Backends

| Feature | ARM64 | x86_64 | WebAssembly |
|---------|-------|--------|-------------|
| Register allocation | ✅ | ✅ | N/A (stack-based) |
| Calling convention | ABI-specific | ABI-specific | Wasm spec |
| Memory model | Flat address space | Flat address space | Linear memory |
| Function calls | `bl` / `call` | `call` | `call` / `call_indirect` |
| Output format | `.s` assembly | `.s` assembly | `.wasm` + `.wat` |
| External assembler | `as` + `ld` | `as` + `ld` | None (direct binary) |
| Portability | ARM64 only | x86_64 only | Universal |
| Runtime | OS | OS | Browser/Node/WASI |

**Key advantage of Wasm:** No external tools needed, universal runtime support!
