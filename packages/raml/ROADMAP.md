# RAML Implementation Roadmap

**Goal:** Build a complete OCaml compiler from scratch, one pass at a time.

---

## ✅ Phase 1: Foundation (COMPLETE!)

**What we built:**
- Complete type system with clear naming
- Hindley-Milner type inference
- Let-polymorphism with Rémy's algorithm  
- Pattern type checking
- Expression type checking for:
  - Constants, variables, let bindings
  - Functions and application
  - Pattern matching
  - If/then/else, tuples
- Working CLI tool (`raml typed-tree --json`)

**Stats:**
- 9 modules, ~1700 lines
- 100% documented
- 0% global state
- Production-ready type checker!

---

## 🎯 Phase 2: Lambda IR (NEXT - ~3 sessions)

**Goal:** Create intermediate representation and translate from TypedTree

### Modules to Implement

#### 1. Lambda IR (`lambda.ml`)
```ocaml
type lambda =
  | Lvar of Identifier.t
  | Lconst of constant  
  | Lapply of { fn : lambda; args : lambda list }
  | Lfunction of { params : Identifier.t list; body : lambda }
  | Llet of { id : Identifier.t; value : lambda; body : lambda }
  | Lletrec of (Identifier.t * lambda) list * lambda
  | Lprim of primitive * lambda list
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lswitch of { scrutinee : lambda; cases : (int * lambda) list }
```

**Primitives:**
```ocaml
type primitive =
  | Pint_add | Pint_sub | Pint_mul | Pint_div
  | Pint_lt | Pint_le | Pint_gt | Pint_ge | Pint_eq
  | Pmakeblock of int (* tag *)
  | Pfield of int
  | Psetfield of int
```

**Size:** ~200 lines

#### 2. Translation (`translateCore.ml`)
- Translate TypedTree expressions to Lambda
- Handle function currying (multi-arg → nested functions)
- Translate patterns to switches
- Generate primitive operations

**Key functions:**
```ocaml
val translate_expression : TypedTree.expression -> lambda
val translate_pattern : TypedTree.pattern -> int * (Identifier.t list)
val translate_constant : TypedTree.constant -> constant
```

**Size:** ~400 lines

#### 3. Pattern Matching (`matching.ml` - simplified)
- Compile patterns to decision trees
- Generate switch statements
- Handle simple patterns (literals, wildcards, vars)
- Skip: guards, or-patterns, complex nesting (for now)

**Size:** ~300 lines

### Deliverable

```bash
raml lambda --json input.ml
```

Outputs:
```json
{
  "lambda": {
    "type": "Llet",
    "id": "x",
    "value": { "type": "Lconst", "value": 42 },
    "body": { "type": "Lvar", "id": "x" }
  }
}
```

**Estimated effort:** 900 lines, 3 sessions

---

## 🎯 Phase 3: Vertical Slice - Direct ARM64! (~5 sessions)

**Goal:** End-to-end compilation, skipping most optimizations

### The Plan: KISS (Keep It Super Simple)

**Skip these (for now):**
- ❌ Closure conversion (only compile top-level functions)
- ❌ Register allocation (use unlimited virtual registers)
- ❌ Instruction scheduling
- ❌ Optimizations
- ❌ GC (leak memory - who cares for demos!)

**Implement these:**
- ✅ Lambda → ARM64 direct translation
- ✅ Simple stack frame management
- ✅ Function calls (leaf functions only)
- ✅ Basic arithmetic
- ✅ Assembly emission
- ✅ Call `as` and `ld` to create executable

### Modules to Implement

#### 1. ARM64 Instructions (`arm64.ml`)
```ocaml
type register = X0 | X1 | X2 | ... | X30 | SP | LR
type instruction =
  | MOV of register * operand
  | ADD of register * register * operand
  | SUB of register * register * operand
  | LDR of register * address
  | STR of register * address
  | B of label
  | BL of label
  | RET
```

**Size:** ~150 lines

#### 2. Code Generation (`arm64Gen.ml`)
```ocaml
val compile_lambda : lambda -> instruction list
val compile_constant : constant -> operand
val compile_primitive : primitive -> lambda list -> instruction list
```

**Key idea:** 
- Constants → `MOV` immediate
- Variables → load from stack
- Functions → `BL` instruction
- Let bindings → store to stack
- Primitives → inline ARM64 ops

**Size:** ~400 lines

#### 3. Emission (`arm64Emit.ml`)
```ocaml
val emit_program : instruction list -> string
val write_assembly : string -> Path.t -> unit
val assemble_and_link : Path.t -> Path.t -> unit
```

Generate `.s` file:
```asm
.global _main
.align 2

_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    
    mov x0, #42        ; let x = 42
    
    ldp x29, x30, [sp], #16
    ret
```

**Size:** ~200 lines

#### 4. Runtime (`runtime.c` - minimal!)
```c
#include <stdio.h>
#include <stdlib.h>

// Just enough to run our code
void* caml_alloc(size_t size) {
    return malloc(size);  // NO GC! Leak everything!
}

void caml_print_int(long n) {
    printf("%ld\n", n);
}
```

**Size:** ~100 lines

### Deliverable

```bash
raml compile input.ml -o output
./output
# Prints: 42
```

**What works:**
- Integer constants
- Let bindings
- Simple arithmetic (`+`, `-`, `*`, `/`)
- Top-level functions (no closures)
- Function calls
- Print integers

**What doesn't work (yet):**
- Closures
- Pattern matching (beyond simple cases)
- Memory management
- Modules
- Type constructors
- Everything else!

**Estimated effort:** 750 lines, 5 sessions

---

## 🎯 Phase 4: Expand Capabilities (~10 sessions)

**Once we have end-to-end working, add features incrementally:**

### 4.1 Pattern Matching (~2 sessions)
- Compile switches properly
- Handle tuples in patterns
- Variant constructors
- **Deliverable:** `match` expressions work

### 4.2 Closures (~3 sessions)
- Closure conversion pass
- Capture free variables
- Create closure records
- **Deliverable:** Nested functions and HOFs work

### 4.3 Data Structures (~2 sessions)
- Tuple allocation
- Variant constructors
- Record types
- **Deliverable:** Can create and use data structures

### 4.4 Module System (~3 sessions)
- Separate compilation
- Module linking
- Module signatures
- **Deliverable:** Multi-file programs work

---

## 🎯 Phase 5: Optimizations (~15 sessions)

**Make it fast!**

### 5.1 Lambda Optimizations (~3 sessions)
- Constant folding
- Dead code elimination
- Inline small functions
- Beta reduction
- **Deliverable:** `raml compile -O1` works

### 5.2 Register Allocation (~5 sessions)
- Liveness analysis
- Graph coloring
- Spilling decisions
- **Deliverable:** Fewer stack operations, faster code

### 5.3 Instruction Selection (~2 sessions)
- Peephole optimizations
- Use specialized ARM64 instructions
- Combine operations
- **Deliverable:** Better assembly generation

### 5.4 Advanced Optimizations (~5 sessions)
- Common subexpression elimination
- Loop optimizations
- Tail call optimization
- **Deliverable:** Competitive with OCaml `-O2`

---

## 🎯 Phase 6: Production Features (~20 sessions)

**Make it reliable!**

### 6.1 Garbage Collection (~8 sessions)
- Copying GC
- Generational GC
- Write barriers
- **Deliverable:** No memory leaks!

### 6.2 Exception Handling (~3 sessions)
- Try/catch compilation
- Stack unwinding
- Exception values
- **Deliverable:** Exceptions work

### 6.3 Debugging (~4 sessions)
- DWARF debug info
- Source location tracking
- Backtrace generation
- **Deliverable:** `gdb` integration

### 6.4 Better Errors (~3 sessions)
- Error recovery in parser
- Better type error messages
- Suggest fixes
- **Deliverable:** User-friendly compiler

### 6.5 Build System (~2 sessions)
- Dependency tracking
- Incremental compilation
- Package management
- **Deliverable:** Fast rebuilds

---

## Alternative Paths

### Path A: Bytecode First (Easier!)
Instead of ARM64, implement bytecode:
- Stack-based VM
- Simple instruction set
- Portable
- Easier to debug

**Tradeoff:** Less exciting, but faster to complete

### Path B: WASM Target
Target WebAssembly instead of ARM64:
- Portable
- Browser integration
- Simpler than native
- Large ecosystem

**Tradeoff:** Different constraints, still interesting

---

## Metrics & Goals

### Phase 2 (Lambda IR)
- **Lines:** ~900
- **Time:** 3 sessions
- **Test:** Translate simple programs

### Phase 3 (Vertical Slice)
- **Lines:** ~750
- **Time:** 5 sessions  
- **Test:** Compile `let x = 42` to ARM64
- **🎉 Milestone:** End-to-end compiler!

### Phase 4 (Expand)
- **Lines:** ~2000
- **Time:** 10 sessions
- **Test:** Compile non-trivial programs

### Phase 5 (Optimize)
- **Lines:** ~3000
- **Time:** 15 sessions
- **Test:** Benchmark vs OCaml

### Phase 6 (Production)
- **Lines:** ~5000
- **Time:** 20 sessions
- **Test:** Self-hosting possible

---

## Success Criteria

### Phase 2 ✅
- [ ] Lambda IR defined
- [ ] TypedTree → Lambda translation works
- [ ] Pattern matching compilation (simple)
- [ ] JSON output of Lambda
- [ ] Tests pass for simple programs

### Phase 3 ✅ (VERTICAL SLICE MILESTONE!)
- [ ] Compile `let x = 42` to ARM64
- [ ] Compile `let f x = x + 1` and call it
- [ ] Generate working executable
- [ ] Can run on ARM64 Mac
- [ ] Print integers works

### Phase 4 ✅
- [ ] Pattern matching works
- [ ] Closures work
- [ ] Tuples/records work
- [ ] Multi-file compilation works

### Phase 5 ✅
- [ ] Optimizations improve performance
- [ ] Register allocation reduces stack usage
- [ ] Benchmark shows competitive performance

### Phase 6 ✅
- [ ] No memory leaks (GC works)
- [ ] Exceptions work
- [ ] Debugging works
- [ ] Error messages are good
- [ ] Build times are acceptable

---

## Current Status

**Completed:**
- ✅ Phase 1: Foundation (100%)
- ✅ Compiler passes analysis
- ✅ Roadmap defined

**Next Up:**
- ⏭️ Phase 2: Lambda IR
  - Start with `lambda.ml` definition
  - Then `translateCore.ml`
  - Then `matching.ml` (simplified)

**Let's build it!** 🚀
