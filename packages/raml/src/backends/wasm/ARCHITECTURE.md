# WebAssembly Backend Architecture

## Pipeline Overview

```
┌─────────────┐
│ TypedTree   │  ← OCaml semantics (types, patterns, modules)
└──────┬──────┘
       │
       ▼
┌─────────────┐
│  Lambda IR  │  ← Functional IR (closure conversion, optimization)
└──────┬──────┘    Shared with native backends
       │
       ▼
┌─────────────┐
│    WasmIR   │  ← WebAssembly-aware IR
└──────┬──────┘    • Stack-based operations
       │           • Explicit memory management
       │           • Linear memory model
       │           • Function tables (for indirect calls)
       ▼
┌─────────────┐
│   WasmAST   │  ← WebAssembly abstract syntax (WAT format)
└──────┬──────┘    • Structured control flow
       │           • Type annotations
       │           • Module structure
       ▼
┌─────────────┐
│ WebAssembly │  ← Binary (.wasm) or Text (.wat) format
└─────────────┘
```

## WebAssembly Specifics

### Memory Model

WebAssembly uses **linear memory**:
- Single contiguous array of bytes
- Accessed via `load` and `store` instructions
- Must manage our own heap allocation
- No garbage collector (we need to implement one or use reference counting)

### Value Types

WebAssembly has 4 value types:
- `i32` - 32-bit integer
- `i64` - 64-bit integer
- `f32` - 32-bit float
- `f64` - 64-bit float

**Reference types** (GC proposal, not yet stable):
- `anyref` - any reference type
- `funcref` - function reference

### OCaml → Wasm Value Representation

| OCaml Type | Wasm Representation | Memory Layout |
|------------|---------------------|---------------|
| `int` | `i32` (31-bit tagged) | Direct value, LSB = 1 for immediate |
| `float` | `f64` | Boxed in heap (8 bytes aligned) |
| `string` | Pointer to heap | Length (i32) + data bytes |
| `bool` | `i32` | 0 = false, 1 = true (tagged as int) |
| `unit` | `i32` | 0 (tagged as int) |
| Variant | Pointer to heap | Tag (i32) + fields |
| Record | Pointer to heap | Fields in order |
| Tuple | Pointer to heap | Elements in order |
| Array | Pointer to heap | Length (i32) + elements |
| Closure | Pointer to heap | Function ptr + environment |

**Tagging scheme** (like OCaml):
- Immediate values (int, bool, unit): LSB = 1, value shifted left
- Pointers: LSB = 0, word-aligned addresses

### Function Calls

**Direct calls**: `call $function_name`
**Indirect calls** (for closures): 
- Function table: `table funcref`
- Call via index: `call_indirect (type $sig)`

### Memory Management

**Options:**
1. **Manual allocation**: Simple bump allocator + no GC (limited)
2. **Reference counting**: Track refs, free when count = 0
3. **Simple GC**: Mark-and-sweep or copying collector
4. **External GC**: Use host's GC (via reference types proposal)

**For MVP, we'll use:**
- Bump allocator (simple, fast)
- No GC initially (accept memory leaks for now)
- Add GC in later phase

## WasmIR Design

WasmIR is a bridge between Lambda IR and WebAssembly:

**Key features:**
- Stack-based operations (matches Wasm execution model)
- Explicit memory operations (load/store/alloc)
- Function tables for closures
- Block structure (if/else/loop/block)

**Example transformation:**

```ocaml
(* Lambda IR *)
Let { id = x; value = Const (Const_int 42); body = ... }

(* WasmIR *)
WConst (WI32 84)        (* 42 << 1 | 1 (tagged int) *)
WSetLocal x
...

(* Wasm *)
i32.const 84
local.set $x
...
```

## Module Structure

WebAssembly module contains:
- **Types**: Function signatures
- **Functions**: Code
- **Table**: Indirect function calls
- **Memory**: Linear memory
- **Globals**: Global variables
- **Exports**: What's visible to host
- **Imports**: What we need from host (e.g., `console.log`)

**OCaml module → Wasm module:**
- Each top-level function → Wasm function + export
- Closures → Entry in function table
- Module initialization → `start` function

## Runtime Library

Needed runtime functions (imported or generated):

```wasm
;; Memory allocation
(func $alloc (param i32) (result i32))

;; String operations
(func $string_concat (param i32 i32) (result i32))
(func $string_length (param i32) (result i32))

;; Printing (imports from host)
(import "console" "log" (func $print (param i32)))

;; Array operations
(func $array_get (param i32 i32) (result i32))
(func $array_set (param i32 i32 i32))

;; Variant construction
(func $make_variant (param i32 i32) (result i32))  ;; tag, arity
```

## Output Formats

1. **WAT (WebAssembly Text)**: Human-readable S-expressions
   ```wasm
   (module
     (func $add (param $x i32) (param $y i32) (result i32)
       local.get $x
       local.get $y
       i32.add
     )
   )
   ```

2. **WASM (Binary)**: Compact binary format
   - Use external tool: `wat2wasm` (from WABT toolchain)
   - Or implement binary encoder

## Compilation Strategy

### Phase 1: Simple (Current Goal)
- Lambda → WasmIR → WAT text
- Integer arithmetic only
- No GC (memory leaks acceptable)
- Simple bump allocator
- Direct calls only (no closures)

### Phase 2: Complete
- Full closure support (function tables)
- String operations
- Pattern matching (via switch)
- Memory management (bump allocator + optional GC)

### Phase 3: Optimized
- Garbage collector (mark-and-sweep or copying)
- Tail call optimization (when Wasm tail calls stable)
- SIMD operations (for arrays/strings)
- Memory64 support (for large heaps)

## File Organization

```
backend/wasm/
  ├── wasmIR.ml/mli         # WasmIR definition
  ├── wasmAST.ml/mli        # WebAssembly AST (WAT format)
  ├── lambda_to_wasm.ml/mli # Lambda → WasmIR translation
  ├── wasm_to_wat.ml/mli    # WasmIR → WAT text generation
  ├── wasm_runtime.ml/mli   # Runtime library generator
  ├── compile.ml/mli        # Orchestration
  └── ARCHITECTURE.md       # This file
```

## Target Triples

- `wasm32-unknown-unknown` - Standalone Wasm (no OS)
- `wasm32-wasi-unknown` - WASI (WebAssembly System Interface)
- `wasm32-unknown-emscripten` - Emscripten runtime
- `wasm64-unknown-unknown` - 64-bit addressing (future)

## Example Compilation

```ocaml
(* OCaml *)
let add x y = x + y

(* Lambda IR *)
Lfunction {
  params = [x; y];
  body = Lprim (Pint_add, [Lvar x; Lvar y])
}

(* WasmIR *)
WFunction {
  params = [(x, WI32); (y, WI32)];
  result = WI32;
  locals = [];
  body = [
    WGetLocal x;
    WGetLocal y;
    WBinOp (WI32, WAdd);
  ]
}

(* WAT *)
(func $add (param $x i32) (param $y i32) (result i32)
  local.get $x
  local.get $y
  i32.add
)
```

## Future: WebAssembly GC Proposal

When the GC proposal is stable, we can use:
- `struct` types for records/variants
- `array` types for OCaml arrays
- Automatic garbage collection
- Better interop with host GC

This will simplify the runtime significantly!
