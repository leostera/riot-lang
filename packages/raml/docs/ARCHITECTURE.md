# RAML Architecture

Complete system architecture for the RAML OCaml compiler.

## Design Philosophy

### 1. Reimplement Algorithms, Redesign Structure

- **Keep proven algorithms:** Type inference (Hindley-Milner), pattern matching compilation, optimization passes
- **Modernize data structures:** Use `Std.Collections` (HashMap, HashSet, Vector) instead of stdlib
- **Clean interfaces:** Clear module boundaries, no cryptic abbreviations
- **Explicit context:** Thread context through functions, NO GLOBAL STATE

### 2. Library-First Architecture

- No monolithic driver
- Each phase is an independent, composable library
- Can be called multiple times in same process
- Testable in isolation

### 3. Multicore-Ready via Miniriot

- Type-check multiple modules in parallel
- Run optimization passes concurrently
- Parallel code generation per module
- Message-passing between compilation processes

## Module Structure

```
src/
├── typechecker/               # Type System (11 modules, ~2600 lines)
│   ├── identifier.ml          # Unique identifiers (NO global state)
│   ├── modulePath.ml          # Module paths
│   ├── types.ml               # Type representations
│   ├── location.ml            # Source positions
│   ├── typedTree.ml           # Typed AST
│   ├── typeOperations.ml      # Type operations (follow_links, occurs_in_type)
│   ├── unification.ml         # Hindley-Milner unification
│   ├── environment.ml         # Typing environment
│   ├── checker.ml             # Expression type checking
│   └── typechecker.ml         # Module aggregator
│
├── lambda/                    # Lambda IR (570 lines)
│   ├── ir.ml                  # Lambda intermediate representation
│   ├── translateCore.ml       # TypedTree → Lambda translation
│   └── lambda.ml              # Module aggregator
│
└── backends/                  # Code Generation
    ├── backends.ml            # Backend selection logic
    │
    ├── arm64/                 # ARM64 Native Backend
    │   ├── instruction.ml     # ARM64 instruction types
    │   ├── codegen.ml         # Lambda IR → ARM64 instructions
    │   ├── emit.ml            # Instructions → Assembly text
    │   ├── compile.ml         # High-level orchestration
    │   └── arm64.ml           # Module aggregator
    │
    ├── x86_64/                # x86_64 Native Backend
    │   ├── instruction.ml     # x86_64 instruction types
    │   ├── codegen.ml         # Lambda IR → x86_64 instructions
    │   ├── emit.ml            # Instructions → Assembly text
    │   ├── compile.ml         # High-level orchestration
    │   └── x86_64.ml          # Module aggregator
    │
    ├── wasm/                  # WebAssembly Backend
    │   ├── wasmIR.ml          # Lambda IR → Stack-based WasmIR
    │   ├── wasmAST.ml         # WasmIR → WAT text format
    │   ├── wasmBinary.ml      # WasmIR → Binary encoder
    │   ├── compile.ml         # High-level orchestration
    │   └── wasm.ml            # Module aggregator
    │
    ├── js/                    # JavaScript Backend (future)
    │   └── ...
    │
    └── native/                # Shared native backend utilities
        └── ...
```

## Compilation Pipeline

```
┌─────────────────────────────────────────────────────────────┐
│ Input: Source file (.ml)                                    │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ Phase 1: Parsing (via Syn library)                         │
│  • Lexical analysis                                         │
│  • Syntactic analysis                                       │
│  • CST (Concrete Syntax Tree) - lossless representation    │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ Phase 2: Type Checking                                      │
│  • Type inference (Hindley-Milner with let-polymorphism)   │
│  • Unification with occurs check                            │
│  • Environment management (value/type/module bindings)      │
│  • Output: TypedTree (typed AST)                           │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ Phase 3: Lambda IR Translation                             │
│  • TypedTree → Lambda IR                                    │
│  • Closure conversion (future)                              │
│  • Pattern matching compilation (future)                    │
│  • Output: Lambda.Ir.lambda                                 │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ Phase 4: Optimization (future)                             │
│  • Inlining                                                 │
│  • Constant folding                                         │
│  • Dead code elimination                                    │
│  • Common subexpression elimination                         │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ Phase 5: Backend-Specific Code Generation                  │
│                                                             │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐    │
│  │   ARM64      │  │   x86_64     │  │ WebAssembly  │    │
│  │              │  │              │  │              │    │
│  │ Instructions │  │ Instructions │  │   WasmIR     │    │
│  │      ↓       │  │      ↓       │  │   (stack)    │    │
│  │  Assembly    │  │  Assembly    │  │      ↓       │    │
│  │   (.s)       │  │   (.s)       │  │  WAT + WASM  │    │
│  │      ↓       │  │      ↓       │  │ (.wat/.wasm) │    │
│  │   Binary     │  │   Binary     │  │              │    │
│  │   (.o)       │  │   (.o)       │  │              │    │
│  └──────────────┘  └──────────────┘  └──────────────┘    │
└─────────────────────────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ Output: Executable/Module                                   │
│  • Native: Linked binary (via ld)                          │
│  • WebAssembly: .wasm module (ready to run)                │
└─────────────────────────────────────────────────────────────┘
```

## Type System Architecture

### Core Types

```ocaml
type type_expr =
  | Variable of { id : int; mutable link : type_link }
  | Arrow of { param : type_expr; return : type_expr }
  | Tuple of type_expr list
  | Constructor of {
      path : ModulePath.t;
      args : type_expr list;
    }
  | Polymorphic of {
      params : type_expr list;
      body : type_expr;
    }

and type_link =
  | Unbound of { level : int }
  | Linked of type_expr
```

### Type Checking Algorithm

**Hindley-Milner with levels:**
1. **Inference**: Assign fresh type variables
2. **Unification**: Solve type constraints
3. **Generalization**: Promote unbound variables at current level
4. **Instantiation**: Create fresh copies of polymorphic types

**Occurs Check:** Prevents infinite types like `'a = 'a -> 'b`

**Let-polymorphism:**
```ocaml
let id = fun x -> x in   (* id : 'a -> 'a *)
(id 42, id "hello")      (* OK: instantiated twice *)
```

See [docs/typechecker/TYPE_SYSTEM.md](./typechecker/TYPE_SYSTEM.md) for details.

## Lambda IR

**Purpose:** High-level intermediate representation that:
- Abstracts away OCaml-specific surface syntax
- Makes pattern matching explicit
- Represents closures uniformly
- Suitable for optimization and code generation

**Key concepts:**
- Variables (bound identifiers)
- Constants (int, float, string, block)
- Primitives (int_add, int_sub, etc.)
- Let bindings
- Function applications
- Functions (future: closure conversion)
- Pattern matching (future: compilation to switches)

```ocaml
type lambda =
  | Const of constant
  | Var of Identifier.t
  | Let of {
      id : Identifier.t;
      value : lambda;
      body : lambda;
    }
  | Prim of primitive * lambda list
  | Function of {
      params : Identifier.t list;
      body : lambda;
    }
  | Apply of {
      func : lambda;
      args : lambda list;
    }
```

## Backend Architecture

### Common Pattern

All backends follow the same structure:

1. **instruction.ml** - Target-specific instruction types
2. **codegen.ml** - Lambda IR → Instructions
3. **emit.ml** - Instructions → Text (assembly or WAT)
4. **compile.ml** - Orchestrate: codegen → emit → write files

### ARM64 Backend

**Features:**
- Native code generation for Apple Silicon
- Register allocation (simple strategy)
- Calling convention: AAPCS64
- System calls via `svc #0x80`

**Output:** `.s` assembly file → assembled with `as` → linked with `ld`

### x86_64 Backend

**Features:**
- Native code generation for Intel/AMD
- AT&T assembly syntax
- Register allocation
- Calling convention: System V AMD64 ABI

**Output:** `.s` assembly file → assembled with `as` → linked with `ld`

### WebAssembly Backend

**Features:**
- Stack-based IR (WasmIR)
- Custom binary encoder (no external tools!)
- LEB128 integer encoding
- IEEE 754 float encoding
- All Wasm sections: type, function, memory, export, code

**Output:**
- `.wat` - Text format (S-expressions)
- `.wasm` - Binary format (ready to run)

**Key advantage:** No external assembler or linker needed!

See [docs/backends/WEBASSEMBLY.md](./backends/WEBASSEMBLY.md) for details.

## Context Threading

**NO GLOBAL STATE!** All state is explicit:

```ocaml
(* Type checking context *)
type context = {
  type_id_counter : int ref;
  type_level : int;
  identifier_ctx : Identifier.context;
  environment : Environment.t;
}

(* Thread through all functions *)
let check_expression ~ctx expr =
  let typed_expr, new_ctx = ... in
  typed_expr, new_ctx
```

**Benefits:**
- Can run compiler multiple times in same process
- Easy to test (just create fresh context)
- No hidden state
- Thread-safe (each thread has own context)

## Memory Management

### Type System

- Type variables use mutable links for unification
- Managed by OCaml's GC
- No manual memory management needed

### Backend Outputs

**Native (ARM64/x86_64):**
- Generated code uses system stack
- Heap allocation via system malloc (future)
- Garbage collection (future)

**WebAssembly:**
- Linear memory (starts with 1 page = 64KB)
- Stack-based execution
- Heap allocation via bump allocator (future)
- GC via reference counting or mark-and-sweep (future)

## Error Handling

**Use Result types everywhere:**

```ocaml
val type_check : context -> CST.expression -> (TypedTree.expression, error) Result.t

val compile : Path.t -> target -> (unit, string) Result.t
```

**Never use exceptions for control flow!**

## Testing Strategy

### Unit Tests

- Type inference tests: `tests/test_type_checker.ml`
- Type operations: `tests/test_types.ml`
- Identifier handling: `tests/test_ident.ml`

### Integration Tests

49 test fixtures in `tests/fixtures/`:
- `simple/` - Basic expressions
- `functions/` - Function definitions
- `polymorphism/` - Generic types
- `types/` - Type definitions
- `variants/` - Sum types
- `records/` - Product types
- `patterns/` - Pattern matching
- `errors/` - Type errors

### End-to-End Tests

```bash
# Compile and run
echo "let x = 42" > test.ml
raml compile test.ml --target wasm -o test.wasm
node -e "WebAssembly.instantiate(require('fs').readFileSync('test.wasm')).then(o => console.log(o.instance.exports.main()))"
# Expected: 42
```

## Performance Considerations

### Parallel Compilation (Future)

Use Miniriot processes to parallelize:
- Type checking of independent modules
- Optimization passes
- Code generation per module

```ocaml
(* Future API *)
let compile_modules modules =
  modules
  |> List.map (fun m -> 
       Miniriot.spawn (fun () -> compile_one_module m)
     )
  |> List.map Miniriot.await
```

### Optimization Passes (Future)

- Constant folding
- Dead code elimination
- Inlining (small functions)
- Common subexpression elimination
- Tail call optimization

## Standards and Conventions

### Naming

- **Clear names:** `Variable` not `Tvar`
- **Full words:** `TypeOperations` not `Btype`
- **No abbreviations:** `Unification` not `Ctype`

### Module Organization

- Each module: `.ml` + `.mli` (interface always!)
- Aggregator modules: Export submodules
- No cyclic dependencies

### Documentation

- Heavy documentation for complex algorithms
- Examples in comments
- Explain *why*, not just *what*

## Future Directions

### Phase 1: Complete Language Support ✅ In Progress

- Pattern matching compilation
- Closures with environment capture
- Heap allocation for strings/records/tuples
- Module system basics

### Phase 2: Optimizations

- Inlining
- Constant propagation
- Dead code elimination
- Tail call optimization

### Phase 3: Advanced Features

- Functors
- First-class modules
- GADTs
- Modular implicit

### Phase 4: Tooling

- Language server (LSP)
- Debugger integration
- Profiler
- Package manager integration

## References

- **OCaml compiler:** github.com/ocaml/ocaml (typing/ and lambda/ directories)
- **Hindley-Milner:** Damas & Milner (1982) "Principal type-schemes for functional programs"
- **Rémy's algorithm:** Didier Rémy (1992) "Extension of ML Type System with a Sorted Equational Theory on Types"
- **WebAssembly spec:** webassembly.github.io/spec/core/
