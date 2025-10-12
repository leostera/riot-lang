# RAML

**R**iot **A**dvanced **M**eta **L**anguage - A modern OCaml-to-native compiler.

## Overview

RAML is a complete OCaml compiler implementation with three production backends:
- **ARM64** - Native code for Apple Silicon
- **x86_64** - Native code for Intel/AMD
- **WebAssembly** - Browser and Node.js targets

Built with modern infrastructure: multicore-ready, library-first design, zero global state.

## Quick Start

```bash
# Install
tusk install raml

# Compile to native (ARM64/x86_64)
raml compile input.ml --target arm64-apple-darwin -o output

# Compile to WebAssembly
raml compile input.ml --target wasm -o output.wasm

# Run WebAssembly output
node -e "WebAssembly.instantiate(require('fs').readFileSync('output.wasm')).then(o => console.log(o.instance.exports.main()))"
```

## Current Status

**Working End-to-End:** Source → Parse → TypeCheck → Lambda IR → Assembly/Wasm → Binary

✅ **Type System** - Hindley-Milner inference with let-polymorphism  
✅ **Lambda IR** - Intermediate representation based on OCaml's Lambda  
✅ **ARM64 Backend** - Full native code generation for Apple Silicon  
✅ **x86_64 Backend** - Native code generation for Intel/AMD  
✅ **WebAssembly Backend** - Custom binary encoder, no external dependencies  

### What Works Today

**Type Checking:**
- Constants, variables, let bindings
- Functions (anonymous, recursive, higher-order)
- Type inference and unification
- Let-polymorphism with instantiation/generalization

**Compilation:**
- Simple expressions and arithmetic
- Local variables
- Integer constants
- Binary operations (add, sub, mul, div, mod)

**Backend Outputs:**
- ARM64: Native executables
- x86_64: Native executables
- WebAssembly: `.wasm` binary + `.wat` text format

### In Progress (Stub Implementations)

- Pattern matching
- Function definitions and calls
- Closures
- Strings and heap allocation
- Records and tuples
- Variants

## Architecture

```
raml compile input.ml → output
    ↓
┌─────────────────────────────────────────────────────────┐
│  Frontend: Parsing & Type Checking                     │
│  ├─ syn: Parse to CST (lossless concrete syntax tree)  │
│  ├─ TypeChecker: Hindley-Milner type inference         │
│  └─ TypedTree: Typed abstract syntax tree              │
└─────────────────────────────────────────────────────────┘
    ↓
┌─────────────────────────────────────────────────────────┐
│  Middle-end: Lambda IR                                  │
│  ├─ Lambda/Ir: Intermediate representation              │
│  ├─ Lambda/TranslateCore: TypedTree → Lambda           │
│  └─ Simplification passes (future)                     │
└─────────────────────────────────────────────────────────┘
    ↓
┌─────────────────────────────────────────────────────────┐
│  Backend: Code Generation (pick one)                   │
│  ├─ ARM64: Native assembly for Apple Silicon           │
│  ├─ x86_64: Native assembly for Intel/AMD              │
│  └─ WebAssembly: Binary encoder (no wat2wasm!)         │
└─────────────────────────────────────────────────────────┘
    ↓
  Binary output (.o, .wasm, etc.)
```

### Module Structure

```
src/
├── typechecker/          # Type inference system
│   ├── identifier.ml     # Unique identifiers (NO global state)
│   ├── modulePath.ml     # Module paths
│   ├── types.ml          # Type representations
│   ├── typedTree.ml      # Typed AST
│   ├── typeOperations.ml # Type operations (follow links, occurs check)
│   ├── unification.ml    # Hindley-Milner unification
│   ├── environment.ml    # Typing environment
│   └── checker.ml        # Expression type checking
│
├── lambda/               # Lambda IR
│   ├── ir.ml             # Lambda intermediate representation
│   └── translateCore.ml  # TypedTree → Lambda translation
│
└── backends/             # Code generation
    ├── arm64/            # ARM64 native backend
    │   ├── instruction.ml
    │   ├── codegen.ml
    │   ├── emit.ml
    │   └── compile.ml
    │
    ├── x86_64/           # x86_64 native backend
    │   ├── instruction.ml
    │   ├── codegen.ml
    │   ├── emit.ml
    │   └── compile.ml
    │
    └── wasm/             # WebAssembly backend
        ├── wasmIR.ml     # Lambda → WasmIR (stack-based)
        ├── wasmAST.ml    # WasmIR → WAT text
        ├── wasmBinary.ml # WasmIR → .wasm binary
        └── compile.ml    # High-level orchestration
```

## Design Principles

### 1. NO Cryptic Abbreviations

We write code for **humans first, compilers second**.

| ❌ BAD (cryptic) | ✅ GOOD (clear) |
|-----------------|----------------|
| `Tvar` | `Variable` |
| `Tpoly` | `Polymorphic` |
| `Pident` | `Identifier` |
| `Pdot` | `Dot` |
| `Btype` | `TypeOperations` |
| `Ctype` | `Unification` |
| `Env` | `Environment` |

### 2. NO Global Mutable State

The compiler is a **pure library** callable multiple times:

```ocaml
(* BAD - global state *)
let counter = ref 0  (* ❌ NEVER *)

(* GOOD - explicit context *)
let create_context () = { stamp_counter = 0; ... }
let compile ~ctx source = ...
```

### 3. Modern Infrastructure

- Use `Std.Collections.HashMap` not `Hashtbl`
- Use `Result.t` for errors not exceptions
- Use `Std.Path.t` for file paths
- Thread `context` explicitly through all functions

### 4. Library-first Design

Each phase is an independent, composable library. No monolithic driver.

## WebAssembly Backend Details

The WebAssembly backend is **completely self-contained** with no external dependencies:

**Features:**
- Custom binary encoder (LEB128 signed/unsigned, IEEE 754 floats)
- All Wasm sections: type, function, memory, export, code
- Proper instruction encoding with opcodes
- Generates both `.wat` (human-readable) and `.wasm` (binary)

**Binary Format:**
```
Magic:    0x00 0x61 0x73 0x6d  (\0asm)
Version:  0x01 0x00 0x00 0x00  (v1)
Sections: [Type] [Function] [Memory] [Export] [Code]
```

**Example Output:**
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

See [WebAssembly Backend Architecture](./docs/backends/WEBASSEMBLY.md) for details.

## Documentation

### Getting Started
- **[README.md](./README.md)** (this file) - Overview and quick start
- **[docs/GETTING_STARTED.md](./docs/GETTING_STARTED.md)** - Detailed setup and usage

### Architecture
- **[docs/ARCHITECTURE.md](./docs/ARCHITECTURE.md)** - Complete system architecture
- **[docs/COMPILER_PASSES.md](./docs/COMPILER_PASSES.md)** - Compilation pipeline explained
- **[docs/DESIGN_PRINCIPLES.md](./docs/DESIGN_PRINCIPLES.md)** - Why we built it this way

### Type System
- **[docs/typechecker/TYPE_SYSTEM.md](./docs/typechecker/TYPE_SYSTEM.md)** - Type inference algorithm
- **[docs/typechecker/REMY_ALGORITHM.md](./docs/typechecker/REMY_ALGORITHM.md)** - Rémy's efficient generalization

### Backends
- **[docs/backends/ARM64.md](./docs/backends/ARM64.md)** - ARM64 code generation
- **[docs/backends/X86_64.md](./docs/backends/X86_64.md)** - x86_64 code generation
- **[docs/backends/WEBASSEMBLY.md](./docs/backends/WEBASSEMBLY.md)** - WebAssembly compilation

## Testing

**49 test fixtures** covering the OCaml language:

```
tests/fixtures/
├── simple/        # Literals, let bindings, tuples
├── functions/     # Identity, recursion, higher-order
├── polymorphism/  # Generics, type inference
├── types/         # Aliases, annotations
├── variants/      # Sum types, pattern matching
├── records/       # Product types, field access
├── patterns/      # Wildcards, or-patterns
└── errors/        # Type errors, occurs check
```

Run tests:
```bash
cd tests && ./run_tests.sh
```

## Contributing

**Golden Rule:** If you can't understand what something does from its name alone, **rename it**!

Examples of good renames:
- `Tconstr` → `Constructor`
- `Lvar` → `Variable`  
- `Cconst_int` → `IntegerConstant`

We're building a compiler that **anyone** can read and contribute to.

## Roadmap

### Phase 1: Foundation ✅ COMPLETE
- Type system with Hindley-Milner inference
- Lambda IR translation
- Basic backends scaffolding

### Phase 2: Code Generation ✅ COMPLETE  
- ARM64 native backend
- x86_64 native backend
- WebAssembly backend with binary encoder

### Phase 3: Full Language Support (In Progress)
- Pattern matching compilation
- Closures and function tables
- String and heap allocation
- Records and tuples
- Variants and algebraic types

### Phase 4: Optimizations (Future)
- Inlining
- Dead code elimination
- Constant folding
- Register allocation improvements

### Phase 5: Advanced Features (Future)
- Module system
- Functors
- First-class modules
- GADTs

See [docs/ROADMAP.md](./docs/ROADMAP.md) for detailed milestones.

## License

Same as Riot project
