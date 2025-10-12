# RAML Compiler Architecture

Lessons learned from js_of_ocaml/wasm_of_ocaml and designed for multiple compilation targets.

## Overview

RAML compiles OCaml code to multiple targets (native, JavaScript, WebAssembly) by using a shared intermediate representation (IR) with backend-specific transformations.

```
                    ┌─────────────┐
                    │   Source    │
                    │   (.ml)     │
                    └──────┬──────┘
                           │
                           ▼
                    ┌─────────────┐
                    │   Parser    │  ← TODO: Fix Syn or use OCaml's parser
                    └──────┬──────┘
                           │
                           ▼
                    ┌─────────────┐
                    │ TypedTree   │  ← OCaml semantics (types, patterns)
                    └──────┬──────┘
                           │
                           ▼
          ┌────────────────┴────────────────┐
          │         Lambda IR               │  ← Shared IR (like js_of_ocaml's Code IR)
          │  (Functional, high-level)       │
          └────────┬──────────────┬─────────┘
                   │              │
        ┌──────────┴──┐     ┌─────┴──────────┐
        │  Native     │     │  VM/Managed    │
        │  Backends   │     │  Backends      │
        └──────┬──────┘     └─────┬──────────┘
               │                   │
     ┌─────────┼─────────┐    ┌───┴────┐
     │         │         │    │        │
     ▼         ▼         ▼    ▼        ▼
  ┌────┐   ┌────┐   ┌────┐ ┌────┐  ┌────┐
  │ARM │   │x86 │   │RISC│ │Wasm│  │ JS │
  │ 64 │   │_64 │   │ -V │ │ IR │  │Jambda
  └─┬──┘   └─┬──┘   └─┬──┘ └─┬──┘  └─┬──┘
    │        │         │      │       │
    ▼        ▼         ▼      ▼       ▼
  Assembly Assembly Assembly WAT   JsTree
    │        │         │      │       │
    ▼        ▼         ▼      ▼       ▼
  Binary  Binary   Binary  .wasm    .js
```

## Key Insight from js_of_ocaml

**js_of_ocaml is NOT a bytecode interpreter!**

Previously thought:
- ❌ OCaml bytecode → JavaScript VM → Run bytecode

Actually:
- ✅ OCaml bytecode → Parse → **Code IR** → Optimize → JavaScript
- ✅ Code IR is a **three-address code** intermediate representation
- ✅ Real compilation with optimizations (inlining, DCE, etc.)
- ✅ wasm_of_ocaml **shares** the Code IR, just different backend

## RAML's IR Layers

### 1. Lambda IR (Shared Core)

**Purpose:** Functional IR shared between all backends

**Inspired by:** OCaml's Lambda IR, js_of_ocaml's Code IR

**Key features:**
- Closure conversion
- Pattern match compilation
- Curried function applications
- High-level primitives

**Used by:** ALL backends (native and VM)

**File:** `src/lambda/ir.ml`

```ocaml
type lambda =
  | Var of Identifier.t
  | Const of constant
  | Apply of { func; args }
  | Function of { params; body }
  | Let of { id; value; body }
  | Prim of primitive * lambda list
  ...
```

### 2. Backend-Specific IRs

Each backend can add its own IR layer for target-specific transformations.

#### 2a. Native Backends (ARM64, x86_64, RISC-V)

**Pipeline:** `Lambda IR → Assembly → Binary`

**No intermediate IR needed** - Lambda compiles directly to assembly instructions.

**Why?** Native compilation is straightforward:
- Lambda operations → Machine instructions
- Register allocation
- Calling conventions

**Files:**
- `src/codegen/arm64/instruction.ml` - ARM64 instructions
- `src/codegen/arm64/codegen.ml` - Lambda → ARM64
- `src/codegen/x86_64/instruction.ml` - x86-64 instructions
- `src/codegen/x86_64/codegen.ml` - Lambda → x86-64

#### 2b. JavaScript Backend

**Pipeline:** `Lambda IR → Jambda → JsTree → JavaScript`

**Jambda** = JavaScript-aware IR (like js_of_ocaml's js backend transformations)

**Purpose:**
- Uncurrying optimization (`f(a)(b)(c)` → `f(a,b,c)`)
- Runtime representation decisions (how OCaml values map to JS)
- JS-specific primitives (array access, object operations)

**JsTree** = JavaScript AST (1:1 with JS syntax)

**Purpose:**
- Pretty-printing to JS
- Source map generation
- Multiple module formats (ES6, CommonJS, IIFE)

**Files:**
- `src/codegen/js/jambda.ml` - JS-aware IR
- `src/codegen/js/jstree.ml` - JavaScript AST
- `src/codegen/js/ARCHITECTURE.md` - Detailed docs

#### 2c. WebAssembly Backend

**Pipeline:** `Lambda IR → WasmIR → WasmAST → .wat/.wasm`

**WasmIR** = WebAssembly-aware IR (like wasm_of_ocaml's wasm backend)

**Purpose:**
- Stack-based operations
- Memory management (linear memory, heap allocation)
- Function tables (for indirect calls/closures)
- GC integration (using Wasm GC proposal when stable)

**WasmAST** = WebAssembly AST (WAT format)

**Purpose:**
- Generate .wat text format
- Binary .wasm encoding (via wat2wasm)
- Structured control flow

**Files:**
- `src/codegen/wasm/wasmIR.ml` - Wasm-aware IR
- `src/codegen/wasm/wasmAST.ml` - WebAssembly AST
- `src/codegen/wasm/ARCHITECTURE.md` - Detailed docs

## Comparison with js_of_ocaml/wasm_of_ocaml

| Aspect | js_of_ocaml | RAML |
|--------|-------------|------|
| **Input** | OCaml bytecode (`.cmo`) | TypedTree (AST) |
| **Shared IR** | Code IR (three-address) | Lambda IR (functional) |
| **JS Backend** | Code → JavaScript | Lambda → Jambda → JsTree → JS |
| **Wasm Backend** | Code → WasmIR → Wasm | Lambda → WasmIR → Wasm |
| **Native** | ❌ None | ✅ ARM64, x86-64 |
| **Optimizations** | In Code IR | In Lambda IR + backend-specific |

## Why Different from js_of_ocaml?

1. **Start from TypedTree, not bytecode**
   - We have type information available
   - Can do type-directed optimizations
   - Don't need to parse bytecode format

2. **Support native compilation**
   - js_of_ocaml only targets JS/Wasm
   - We target native (ARM64, x86-64) + VM (JS, Wasm)

3. **Simpler shared IR**
   - Lambda IR is functional (matches OCaml semantics)
   - js_of_ocaml's Code IR is imperative (three-address code)
   - Trade-off: Our backend transformations do more work

## Compilation Modes

### Mode 1: Native Compilation

```bash
raml compile --target aarch64-apple-darwin -o program
```

**Pipeline:**
1. Parse `.ml` → TypedTree
2. TypedTree → Lambda IR
3. Lambda IR → ARM64 assembly
4. Assemble → Binary

**Current status:** ✅ Working for simple programs (constants)

### Mode 2: JavaScript Compilation

```bash
raml compile --target js-ecma-unknown -o program.js
```

**Pipeline:**
1. Parse `.ml` → TypedTree
2. TypedTree → Lambda IR
3. Lambda IR → Jambda (uncurrying, runtime representations)
4. Jambda → JsTree (JavaScript AST)
5. JsTree → JavaScript code

**Current status:** 🚧 Architecture designed, not implemented

### Mode 3: WebAssembly Compilation

```bash
raml compile --target wasm32-unknown-unknown -o program.wasm
```

**Pipeline:**
1. Parse `.ml` → TypedTree
2. TypedTree → Lambda IR
3. Lambda IR → WasmIR (stack ops, memory, function tables)
4. WasmIR → WasmAST (WAT format)
5. WasmAST → Binary .wasm

**Current status:** 🚧 Architecture designed, not implemented

## Target Triples

### Native Targets
- `aarch64-apple-darwin` - Apple Silicon (ARM64 macOS) ✅
- `arm64-apple-darwin` - Alias for aarch64-apple-darwin ✅
- `x86_64-apple-darwin` - Intel macOS ✅
- `x86_64-unknown-linux-gnu` - x86-64 Linux 🚧
- `riscv64gc-unknown-linux-gnu` - RISC-V 64-bit 🚧

### JavaScript Targets
- `js-ecma-unknown` - Standard ECMAScript (portable) 🚧
- `js-ecma-node` - Node.js-specific APIs 🚧
- `js-ecma-browser` - Browser-specific APIs 🚧

### WebAssembly Targets
- `wasm32-unknown-unknown` - Standalone Wasm 🚧
- `wasm32-wasi-unknown` - WASI (WebAssembly System Interface) 🚧
- `wasm64-unknown-unknown` - 64-bit addressing (future) 🚧

## Optimizations

### Lambda IR Level (Shared)
- Constant folding
- Dead code elimination
- Inlining (small functions)
- Closure conversion

### Backend-Specific
- **Native:** Register allocation, instruction selection
- **JavaScript:** Uncurrying, flattening closures
- **WebAssembly:** Stack optimization, memory layout

## Future Work

1. **Parser integration** - Fix Syn or use OCaml's parser
2. **Complete backends:**
   - JavaScript (Jambda → JsTree → JS)
   - WebAssembly (WasmIR → WAT/Wasm)
3. **Garbage collection:**
   - Native: Add simple GC (copying or mark-sweep)
   - Wasm: Use Wasm GC proposal when stable
   - JS: Let JS handle it
4. **Runtime library:**
   - Currying helpers
   - String/array operations
   - Exception handling
5. **Optimizations:**
   - Tail call optimization
   - SIMD for arrays
   - Dead code elimination per backend

## Directory Structure

```
raml/
├── src/
│   ├── typechecker/          # Type checking (Phase 1) ✅
│   ├── lambda/               # Lambda IR (Phase 2) ✅
│   │   ├── ir.ml            # Lambda IR definition
│   │   └── translateCore.ml # TypedTree → Lambda
│   └── codegen/             # Code generation (Phase 3)
│       ├── arm64/           # ARM64 backend ✅
│       ├── x86_64/          # x86-64 backend ✅
│       ├── js/              # JavaScript backend 🚧
│       │   ├── jambda.ml   # JS-aware IR
│       │   ├── jstree.ml   # JavaScript AST
│       │   └── ARCHITECTURE.md
│       └── wasm/            # WebAssembly backend 🚧
│           ├── wasmIR.ml   # Wasm-aware IR
│           ├── wasmAST.ml  # WebAssembly AST
│           └── ARCHITECTURE.md
└── COMPILER_ARCHITECTURE.md # This file
```

## References

- **js_of_ocaml:** [GitHub](https://github.com/ocsigen/js_of_ocaml)
  - Code IR: `compiler/lib/code.ml`
  - Shows how to do real compilation (not interpretation!)
  
- **wasm_of_ocaml:** Fork of js_of_ocaml for WebAssembly
  - WasmIR: `compiler/lib-wasm/wasm_ast.ml`
  - Uses WebAssembly GC proposal
  
- **OCaml compiler:** Our Lambda IR inspired by OCaml's
  - Lambda: `ocaml/lambda/lambda.ml`
