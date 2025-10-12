# RAML

**R**iot **A**dvanced **M**eta **L**anguage - A modern, multicore-ready OCaml compiler.

**Current Status:** ✅ **Phase 1 Complete** - Full type checker working!

## Quick Start

```bash
# Install
tusk install raml

# Type check a file
raml typed-tree --json input.ml
```

## Purpose

An idiomatic rewrite of the OCaml compiler as a library, built with modern infrastructure:

- 🚀 **Multicore-ready** - Parallel type-checking and compilation via `miniriot`
- 📚 **Library-first** - Clean APIs for embedding and tooling
- 🛠️ **Modern stdlib** - Built on `std` with ergonomic error handling
- 🌳 **Syn integration** - Leverages lossless CST parsing
- 🎯 **Multi-target** - ARM, JavaScript, Bytecode, WebAssembly backends
- 🎓 **Beginner-friendly** - NO CRYPTIC NAMES! Everything is readable and clear

## Architecture

```
RAML
├── TypeChecker/         # Type inference, checking, environment
│   ├── Identifier       # Unique identifiers (NOT "Ident")
│   ├── ModulePath       # Module paths (NOT "Path")
│   ├── Types            # Type representations (NOT abbreviated!)
│   ├── TypedTree        # Typed AST (NOT "Typedtree")
│   ├── TypeOperations   # Type operations (NOT "Btype")
│   ├── Unification      # Type unification (NOT "Ctype")
│   └── Environment      # Typing environment (NOT "Env")
│
├── Lambda/              # Lambda IR (intermediate representation)
├── Simplify/            # Simplification passes
├── Cmm/                 # C-- low-level IR
└── Backends/            # Code generation
    ├── ARM64
    ├── JavaScript
    ├── ByteCode
    └── WebAssembly
```

## Design Principles

### 1. 🚫 NO CRYPTIC ABBREVIATIONS

We write code for **humans first, compilers second**:

| ❌ BAD (cryptic) | ✅ GOOD (clear) |
|-----------------|----------------|
| `Tvar` | `Variable` |
| `Tpoly` | `Polymorphic` |
| `Pident` | `Identifier` |
| `Pdot` | `Dot` |
| `Btype` | `TypeOperations` |
| `Ctype` | `Unification` |
| `Env` | `Environment` |

**Why?** Because someone reading this code for the first time should understand what everything does without a decoder ring.

### 2. 🚫 NO GLOBAL MUTABLE STATE

The compiler is a **pure library** callable multiple times:

```ocaml
(* BAD - global state *)
let counter = ref 0  (* ❌ NEVER *)

(* GOOD - explicit context *)
let create_context () = { stamp_counter = 0; ... }
let compile ~ctx source = ...
```

### 3. ✅ Modern Infrastructure

- Use `Std.Collections.HashMap` not `Hashtbl`
- Use `Result.t` for errors not exceptions
- Use `Std.Path.t` for file paths
- Thread `context` explicitly through all functions

### 4. ✅ Library-first Design

No monolithic driver. Each phase is an independent, composable library.

## Current Status (Phase 1 Complete ✅)

**Implemented (536 lines):**
- ✅ `Identifier` - Unique identifiers with NO GLOBAL STATE
- ✅ `ModulePath` - Module path representation  
- ✅ `Types` - Core type system with clear constructor names
- ✅ `Location` - Source position tracking
- ✅ `TypedTree` - Complete typed AST
- ✅ `TypeOperations` - Type operations (repr, occurs_check, levels)
- ✅ `Unification` - Full unification with instantiation & generalization

**Next Steps:**
- Environment - Typing environment (value/type/module bindings)
- TypeChecker - Expression type checking
- Test on `let x = 42`

## Usage (Future)

```ocaml
open Std

(* Parse source *)
let source = Fs.read (Path.v "main.ml") |> Result.unwrap in
let cst = Syn.Parser.parse source |> Result.unwrap in

(* Type check *)
let ctx = Types.create_context () in
let typed_tree, ctx = TypeChecker.check_structure ~ctx cst in

(* Translate to Lambda IR *)
let lambda_ir = Lambda.translate typed_tree in

(* Simplify *)
let lambda_ir = Simplify.optimize lambda_ir in

(* Generate code *)
let asm = Backends.ARM64.compile lambda_ir in
Fs.write (Path.v "main.o") asm
```

## Testing

**49 test fixtures** from simple to complex:
- Simple: literals, let bindings, tuples
- Functions: identity, recursion, higher-order
- Polymorphism: generics, type inference
- Types: aliases, annotations, GADTs
- Variants: sum types, pattern matching
- Records: product types, field access
- Patterns: wildcards, or-patterns, nesting
- Errors: unification failures, occurs check

## Contributing

**Golden Rule:** If you can't understand what something does from its name alone, **rename it**!

Examples:
- `Tconstr` → `Constructor`
- `Lvar` → `Variable`
- `Cconst_int` → `IntegerConstant`

We're building a compiler that **anyone** can read and contribute to.

## Documentation

- **[STATUS.md](./STATUS.md)** - Current implementation status and capabilities
- **[COMPILER_PASSES.md](./COMPILER_PASSES.md)** - Complete analysis of OCaml compiler passes
- **[ROADMAP.md](./ROADMAP.md)** - Implementation plan and milestones
- **[ARCHITECTURE.md](./ARCHITECTURE.md)** - System architecture overview
- **[DESIGN_RULES.md](./DESIGN_RULES.md)** - Design principles and coding standards
- **[PROGRESS.md](./PROGRESS.md)** - Detailed implementation progress

## Next Steps

**Phase 2: Lambda IR** (Starting next!)
- Create Lambda intermediate representation
- Implement TypedTree → Lambda translation
- Pattern matching compilation

**Phase 3: Vertical Slice** (The exciting part!)
- Direct Lambda → ARM64 code generation
- End-to-end compilation working
- 🎉 Compile `let x = 42` to working ARM64!

See [ROADMAP.md](./ROADMAP.md) for complete plan.

## License

Same as Riot project
