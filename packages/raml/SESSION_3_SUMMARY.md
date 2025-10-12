# Session 3 Summary: NativeLambda IR Layer Added

## What We Did

### 1. Fixed Tusk Module Compilation Bug

**Problem:** Tusk was auto-generating `backends.mli` referencing submodules before they compiled

**Solution:** Created explicit module aggregators:
- `src/backends/backends.ml`
- `src/backends/backends.mli`

This mirrors the pattern used in `lambda/lambda.ml` and works around the Tusk bug (tracked in RIOT-56).

### 2. Added NativeLambda IR (Corresponds to OCaml's Clambda)

Created a new intermediate representation for native backends:

**Files created:**
- `src/backends/native/nativeLambda.ml` - NativeLambda IR implementation
- `src/backends/native/nativeLambda.mli` - Interface
- `src/backends/native/native.ml` - Module aggregator
- `src/backends/native/native.mli` - Interface
- `src/backends/native/ARCHITECTURE.md` - Detailed documentation

**Purpose:** NativeLambda sits between Lambda IR and native assembly, making closure conversion and memory layout explicit.

## Understanding OCaml's IR Layers

We clarified the full OCaml compiler pipeline:

### OCaml Native Compilation Pipeline

```
Source (.ml)
    ↓
Parser → Typed AST
    ↓
Lambda IR                    ← High-level functional IR
    ↓                          - Pattern matching (high-level)
    ↓                          - Closures (implicit)
    ↓                          - Shared by ALL backends
┌───┴─────────────┐
│   OPTIONAL:     │
│   Flambda       │          ← Advanced optimizer (-O3)
│   (optional)    │            - Cross-module inlining
└───┬─────────────┘            - Specialization
    ↓
Clambda                      ← "Closed Lambda" (Native only!)
    ↓                          - Closures EXPLICIT
    ↓                          - Direct/indirect calls explicit
    ↓                          - Memory layout decided
    ↓
CMM                          ← C-- like imperative IR
    ↓                          - Three-address code
    ↓                          - Very low-level
    ↓
Assembly → Binary
```

### Key Insights

1. **Lambda IR is shared:** Used by native, bytecode, AND (historically) js_of_ocaml
2. **Flambda is optional:** Advanced optimizer, we can skip initially
3. **Clambda is important:** Makes closure conversion explicit - we NEED this for native backends
4. **CMM is too low-level:** We skip this and go directly to assembly

## RAML's IR Strategy

### Phase 1 (Current):
```
TypedTree → Lambda IR → Assembly (ARM64/x86_64)
                      → JsTree (JavaScript)
                      → WasmAST (WebAssembly)
```

### Phase 2 (With NativeLambda - Now Implemented):
```
TypedTree → Lambda IR → NativeLambda → Assembly (Native)
                      ↘ Jambda → JsTree (JS)
                      ↘ WasmIR → WasmAST (Wasm)
```

**Why NativeLambda?**
- Shares closure conversion logic across ARM64/x86_64/RISC-V
- Simplifies backend code generation
- Cleaner separation of concerns

## NativeLambda Features

### 1. Explicit Closures

**Lambda IR (implicit):**
```ocaml
Function { params = [x]; body = Add(x, y) }  (* y is free - implicit! *)
```

**NativeLambda (explicit):**
```ocaml
Uclosure {
  functions = [{ label = "fun_1"; params = [x]; body = ... }];
  free_vars = [y]  (* Captured environment EXPLICIT! *)
}
```

### 2. Direct vs Generic Application

**Lambda IR:**
```ocaml
Apply { func = f; args = [x, y] }  (* All the same *)
```

**NativeLambda:**
```ocaml
Udirect_apply ("fun_label", [x; y])     (* Direct call - fast! *)
Ugeneric_apply (closure_var, [x; y])    (* Indirect - through closure *)
```

### 3. Memory Layout Explicit

```ocaml
Pmakeblock (tag, Mutable/Immutable)  (* Heap allocation *)
Pfield n                              (* Field at offset n *)
Psetfield (n, is_ptr)                (* Field mutation with GC info *)
```

## Implementation Details

### Key Types

```ocaml
type variable = {
  var_name : string;
  var_id : int;      (* Unique ID for SSA-like transforms *)
}

type closure = {
  functions : closure_function list;  (* Mutually recursive functions *)
  free_vars : variable list;          (* Captured environment *)
}

type ulambda =
  | Uvar of variable
  | Uconst of constant
  | Udirect_apply of function_label * ulambda list
  | Ugeneric_apply of ulambda * ulambda list
  | Uclosure of closure
  | Uprim of primitive * ulambda list
  | ...
```

### Transformation: Lambda → NativeLambda

The `NativeLambda.from_lambda` function performs:

1. **Variable conversion** - Map Identifier.t to variable records
2. **Closure identification** - Find free variables (TODO: not yet implemented)
3. **Application splitting** - Direct vs generic (basic implementation)
4. **Primitive lowering** - Map high-level primitives to low-level ones
5. **Pattern match compilation** - Switch compilation (TODO)

### Current Limitations

- ✅ Basic structure working
- ✅ All Lambda constructors handled
- ✅ Primitive conversion
- ⚠️ Closure free variable analysis NOT YET implemented
- ⚠️ Pattern match compilation (Switch) stubbed out
- ⚠️ Not yet integrated with ARM64/x86_64 backends

## Next Steps

### Immediate (Complete NativeLambda):

1. **Implement closure free variable analysis**
   - Analyze function bodies to find captured variables
   - Populate `free_vars` field correctly

2. **Implement pattern match compilation**
   - Convert Lambda `Switch` to NativeLambda `Uswitch`
   - Build decision tree representation

3. **Optimize direct application detection**
   - Detect known function calls for `Udirect_apply`
   - Currently all applications use `Ugeneric_apply`

### Medium Term (Integrate with Backends):

4. **Update ARM64 backend to use NativeLambda**
   - Change `Codegen.compile` to accept `ulambda` instead of `lambda`
   - Use explicit closure info for code generation

5. **Update x86_64 backend to use NativeLambda**
   - Same transformation as ARM64

6. **Add NativeLambda optimization passes**
   - Inline expansion
   - Tail call optimization
   - Closure optimization (eliminate unused free vars)

### Long Term:

7. **Implement Lambda optimization passes**
   - Port OCaml's `simplif.ml` (constant folding, etc.)
   - Port `matching.ml` (optimize pattern matches)

8. **Add more sophisticated closure conversion**
   - Closure sharing for mutually recursive functions
   - Flat closures optimization

## Build Status

✅ All code compiles
✅ No regressions in existing functionality
✅ ARM64/x86_64 backends still work (not using NativeLambda yet)

## Files Modified/Created

### New Files:
- `src/backends/native/nativeLambda.ml` (511 lines)
- `src/backends/native/nativeLambda.mli` (197 lines)
- `src/backends/native/native.ml`
- `src/backends/native/native.mli`
- `src/backends/native/ARCHITECTURE.md` (comprehensive docs)

### Modified Files:
- `src/backends/backends.ml` - Added Native module
- `src/backends/backends.mli` - Added Native module

## Architecture Summary

```
RAML Compilation Pipeline (with NativeLambda)
==============================================

Source.ml
    ↓
[Parser - TODO: Fix Syn or use OCaml parser]
    ↓
TypedTree (typechecker/)
    ↓
Lambda IR (lambda/)              ← Shared by ALL backends
    ↓                              - High-level functional
    ↓                              - Closures implicit
    ↓                              - Pattern matching high-level
    ↓
    ├─→ NativeLambda (backends/native/) ← NEW! For native backends
    │       ↓                              - Closures explicit
    │       ↓                              - Direct/generic calls split
    │       ↓                              - Memory layout decided
    │       ├─→ ARM64 Assembly
    │       ├─→ x86_64 Assembly
    │       └─→ RISC-V Assembly (TODO)
    │
    ├─→ Jambda (backends/js/)    ← For JavaScript
    │       ↓
    │   JsTree → JavaScript
    │
    └─→ WasmIR (backends/wasm/)  ← For WebAssembly
            ↓
        WasmAST → .wat/.wasm
```

## Comparison with OCaml

| IR Layer | OCaml | RAML | Purpose |
|----------|-------|------|---------|
| Lambda | ✅ Yes | ✅ Yes | Shared high-level IR |
| Flambda | ✅ Optional | ❌ Skip (for now) | Advanced optimization |
| Clambda | ✅ Yes | ✅ NativeLambda | Closure conversion |
| CMM | ✅ Yes | ❌ Skip | We go directly to assembly |
| Code IR | ❌ (js_of_ocaml only) | Similar to Jambda/WasmIR | VM backends |

## Learning Resources Created

The `ARCHITECTURE.md` file provides:
- Detailed explanation of NativeLambda
- Comparison with Lambda IR
- Closure representation
- Memory operation primitives
- Pattern match compilation strategy
- Benefits and design rationale

## Summary

We successfully added a **NativeLambda** intermediate representation that corresponds to OCaml's **Clambda**. This IR sits between Lambda and native assembly, making closure conversion and memory layout decisions explicit.

**Key achievement:** We now have a clean architecture that:
1. Shares high-level optimizations in Lambda IR
2. Separates closure conversion (NativeLambda) from assembly generation
3. Allows easy addition of new native backends
4. Maintains compatibility with VM backends (JS, Wasm)

**Status:** Structure complete, basic transformation working. Next step is to implement free variable analysis and integrate with ARM64/x86_64 backends.
