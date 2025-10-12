# RAML Architecture

**R**iot **A**dvanced **M**eta **L**anguage - Modern OCaml compiler rewrite.

## Design Philosophy

1. **Reimplement algorithms, redesign structure**
   - Keep proven algorithms (type inference, pattern match compilation, optimization passes)
   - Modernize data structures (use `std` collections, proper error types)
   - Clean up interfaces and module boundaries

2. **Library-first architecture**
   - No monolithic driver
   - Each phase is an independent library
   - Composable, testable, embeddable

3. **Multicore-ready via processes**
   - Type-check multiple modules in parallel
   - Run optimization passes concurrently
   - Parallel code generation per module

## Module Structure

```
packages/raml/src/
├── typechecker/          # Type inference and checking
│   ├── types.ml          # Core type representations
│   ├── env.ml            # Typing environment
│   ├── typeCore.ml       # Expression type checking
│   ├── typeDecl.ml       # Declaration type checking
│   ├── typeMod.ml        # Module type checking
│   ├── ctype.ml          # Type operations (unification, instantiation)
│   ├── btype.ml          # Basic type operations
│   ├── parmatch.ml       # Pattern match exhaustiveness
│   └── ...               # Port from ocaml/compiler/typing/
│
├── passes/               # IR and optimization passes
│   ├── lambda/           # Lambda IR (ocaml/compiler/lambda)
│   │   ├── lambda.ml     # Lambda AST definition
│   │   ├── translCore.ml # Core language → Lambda
│   │   ├── translMod.ml  # Module language → Lambda
│   │   ├── matching.ml   # Pattern match compilation
│   │   ├── simplif.ml    # Lambda simplification
│   │   └── switch.ml     # Switch optimization
│   │
│   ├── middle_end/       # Middle-end optimizations
│   │   ├── inline.ml     # Function inlining
│   │   ├── constant.ml   # Constant propagation
│   │   ├── deadcode.ml   # Dead code elimination
│   │   ├── closure.ml    # Closure conversion
│   │   └── ...
│   │
│   └── passes.ml         # Pipeline orchestration
│
├── codegen/              # Final AST for code generation
│   ├── cmm.ml            # C-- IR (low-level)
│   ├── cmmGen.ml         # Lambda → CMM
│   └── ...
│
└── backend/              # Target-specific code generation
    ├── bytecode/         # OCaml bytecode backend
    │   ├── byteCode.ml   # Main interface
    │   ├── instruct.ml   # Bytecode instructions
    │   ├── emitcode.ml   # Bytecode emission
    │   └── bytelink.ml   # Bytecode linking
    │
    ├── arm/              # ARM native backend
    │   ├── aRM.ml        # Main interface
    │   ├── emit.ml       # ARM assembly emission
    │   ├── scheduling.ml # Instruction scheduling
    │   └── ...
    │
    ├── javascript/       # JavaScript backend
    │   ├── javaScript.ml # Main interface
    │   ├── jsGen.ml      # JS code generation
    │   └── ...
    │
    └── webassembly/      # WebAssembly backend
        ├── webAssembly.ml
        ├── wasmGen.ml
        └── ...
```

## Compilation Pipeline

```
Source code (string)
    ↓
[Syn.Lexer.tokenize]
    ↓
Token stream
    ↓
[Syn.Parser.parse]
    ↓
CST (Concrete Syntax Tree)
    ↓
[TypeChecker.check]
    ↓
Typed tree
    ↓
[Passes.Lambda.translate]
    ↓
Lambda IR
    ↓
[Passes.optimize]
    ↓
Optimized Lambda IR
    ↓
[CodeGen.to_cmm]
    ↓
CMM (C-- IR)
    ↓
[Backend.*.compile]
    ↓
Target code (bytecode/native/JS/WASM)
```

## Key Data Structures

### TypeChecker

Port from `ocaml/compiler/typing/` with modern infrastructure:

```ocaml
(* Core type representation - similar to Types.type_expr *)
type type_expr = {
  desc: type_desc;
  level: int;
  scope: int;
  id: int;
}

(* Use std collections instead of stdlib *)
module TypeMap = Collections.HashMap
module TypeSet = Collections.HashSet

(* Proper error types instead of exceptions *)
type type_error =
  | Unification_error of type_expr * type_expr
  | Occurs_check of type_expr * type_expr
  | Missing_field of string
  | ...

type check_result = (typed_tree, type_error list) Result.t
```

### Passes.Lambda

Port from `ocaml/compiler/lambda/`:

```ocaml
(* Lambda IR - keep the proven design *)
type lambda =
  | Lvar of ident
  | Lconst of structured_constant
  | Lapply of lambda_apply
  | Lfunction of lfunction
  | Llet of let_kind * value_kind * ident * lambda * lambda
  | Lletrec of (ident * lambda) list * lambda
  | Lprim of primitive * lambda list * location
  | Lswitch of lambda * lambda_switch * location
  | ...

(* Use std collections *)
module IdentMap = Collections.HashMap
module IdentSet = Collections.HashSet
```

### CodeGen.Cmm

Port from `ocaml/compiler/asmcomp/cmm.ml`:

```ocaml
(* C-- intermediate representation *)
type expression =
  | Cconst_int of int
  | Cconst_float of float
  | Cvar of ident
  | Cload of memory_chunk * expression
  | Cstore of memory_chunk * expression * expression
  | Calloc of { bytes : int; ... }
  | ...
```

## Implementation Strategy

### Phase 1: TypeChecker (Critical Path)

1. Port core types (`types.ml`, `btype.ml`, `ctype.ml`)
2. Port typing environment (`env.ml`)
3. Port expression type checking (`typeCore.ml`)
4. Port pattern matching (`parmatch.ml`)
5. Port module typing (`typeMod.ml`)

**Why first?** Type checking is the most complex and valuable component.

### Phase 2: Lambda IR

1. Port Lambda AST (`lambda.ml`)
2. Port pattern match compilation (`matching.ml`, `switch.ml`)
3. Port translation passes (`translCore.ml`, `translMod.ml`)
4. Port simplification (`simplif.ml`)

### Phase 3: Bytecode Backend (Simplest target)

1. Port bytecode instructions (`instruct.ml`)
2. Port bytecode generation (`bytegen.ml`)
3. Port bytecode emission (`emitcode.ml`)
4. Get "hello world" working end-to-end

### Phase 4: Other Backends

- ARM: Port from `ocaml/compiler/asmcomp/arm64/`
- JavaScript: New implementation (reference: js_of_ocaml, rescript)
- WebAssembly: New implementation (reference: wasm_of_ocaml)

## Modernization Points

### Use Std Collections

```ocaml
(* OLD - OCaml stdlib *)
let env = Hashtbl.create 17
Hashtbl.add env key value
match Hashtbl.find_opt env key with ...

(* NEW - Std *)
let env = Collections.HashMap.create ()
HashMap.insert env key value
match HashMap.get env key with ...
```

### Proper Error Handling

```ocaml
(* OLD - Exceptions *)
let type_expr env e =
  try ...
  with Unify err -> raise (Error (env, loc, Expr_type_clash err))

(* NEW - Result *)
let type_expr env e : (typed_expr, type_error) Result.t =
  match unify t1 t2 with
  | Ok () -> ...
  | Error err -> Error (Unification_error (t1, t2))
```

### Process-Based Parallelism

```ocaml
(* Type-check multiple modules in parallel *)
open Miniriot

let type_check_modules modules =
  let workers = List.map (fun m ->
    spawn (fun () -> TypeChecker.check_module m)
  ) modules in
  
  List.map (fun worker ->
    receive ~selector:(function
      | TypeCheckResult r -> `select r
      | _ -> `skip
    ) ()
  ) workers
```

### Path Types

```ocaml
(* Use Std.Path instead of strings for file paths *)
let load_interface name =
  let path = Path.v name / Path.v "interface.cmi" in
  match Fs.read path with
  | Ok content -> parse_cmi content
  | Error e -> Error (Interface_not_found (Path.to_string path))
```

## Reference Implementation

Keep `ocaml/compiler/` as reference for:
- Algorithm correctness (type inference, unification, pattern compilation)
- Edge case handling
- Performance characteristics

Modernize:
- Data structures (use `std` collections)
- Error handling (use `Result` instead of exceptions)
- APIs (clean, composable interfaces)
- Architecture (library-first, process-based parallelism)

## Non-Goals

- ❌ Not maintaining full backward compatibility with OCaml compiler internals
- ❌ Not supporting legacy features (old object system details, etc.)
- ❌ Not building ocamldoc, ocamllex, ocamlyacc (separate tools)
- ❌ Not a drop-in replacement for `ocamlc`/`ocamlopt` (yet)

## Success Criteria

1. **Type-check simple programs correctly** (Phase 1)
2. **Generate working bytecode** (Phase 2-3)
3. **Pass core test suite** (Phase 4)
4. **Parallel compilation works** (Phase 5)
5. **Multiple backends functional** (Phase 6)

## Questions to Answer

1. **Parsetree compatibility?** Use OCaml's Parsetree.t or design our own?
2. **File format compatibility?** Generate compatible .cmi/.cmo/.cmx files?
3. **Standard library?** Compile against OCaml's stdlib or provide our own?
4. **Calling convention?** Match OCaml's runtime or create new one?

## Getting Started

Start with the type checker:

```bash
cd packages/raml
tusk build

# Port a simple file first
cp ../../ocaml/compiler/typing/types.ml src/typechecker/
# Modernize: open Std, use Collections, etc.
```

Then iterate: types → env → ctype → typeCore → ...
