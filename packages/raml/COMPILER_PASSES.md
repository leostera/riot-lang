# OCaml Compiler Passes - Analysis for RAML Implementation

This document analyzes the OCaml compiler pipeline to guide our reimplementation in RAML.

## Overview: The OCaml Compiler Pipeline

OCaml has two main compilation paths:
1. **Bytecode** (`ocamlc`) - Fast compilation, portable bytecode
2. **Native** (`ocamlopt`) - Slower compilation, optimized machine code

Both paths share the **frontend** (parsing + typing), then diverge in the **backend**.

---

## 🎯 Phase 1: FRONTEND (Parsing + Typing)

### 1.1 Parsing Phase (`parsing/`)

**Input:** Source file (`.ml` or `.mli`)  
**Output:** Untyped AST (`Parsetree`)

**Key Files:**
- `lexer.mll` - Tokenization
- `parser.mly` - Grammar rules (Menhir/yacc)
- `parse.ml` - Parse driver
- `parsetree.mli` - AST definition
- `ast_helper.ml` - AST construction helpers
- `pprintast.ml` - Pretty-print AST

**What it does:**
- Lexical analysis (text → tokens)
- Syntactic analysis (tokens → parse tree)
- Handles attributes, docstrings, locations
- Builds untyped AST with source locations

**RAML Status:** ✅ Can use Syn parser (once Syn is fixed)

---

### 1.2 Typing Phase (`typing/`)

**Input:** Untyped AST (`Parsetree`)  
**Output:** Typed AST (`Typedtree`)

**Key Files & Modules:**

#### Core Type System
- `types.mli` - Type representations
- `btype.ml` - Basic type operations (repr, occurs check, levels)
- `ctype.ml` - Type unification, instantiation, generalization
- `ident.ml` - Identifiers with scoping
- `path.ml` - Module paths

#### Environment
- `env.ml` - Type environment (value/type/module bindings)
- `envaux.ml` - Environment helpers
- `persistent_env.ml` - Cross-module environment management

#### Type Checking
- `typecore.ml` - **Main type checker** for expressions
- `typedecl.ml` - Type declaration checking
- `typemod.ml` - Module type checking
- `typeclass.ml` - Class/object type checking

#### Pattern Matching
- `parmatch.ml` - Pattern exhaustiveness/redundancy checking
- `typepat.ml` - Pattern type checking

#### Module System
- `includemod.ml` - Module inclusion checking
- `mtype.ml` - Module type operations
- `subst.ml` - Type substitutions

#### Data Representation
- `datarepr.ml` - Decide memory layout for variants/records
- `primitive.ml` - Primitive operations

**What it does:**
- **Hindley-Milner type inference** with let-polymorphism
- Type checking expressions, patterns, declarations
- Module system (signatures, functors, includes)
- Pattern match exhaustiveness checking
- **Rémy's level-based generalization** algorithm
- Data representation decisions (box vs inline, tag values)

**RAML Status:** ✅ Core type checker complete! (Phase 2 done)

---

## 🔄 Phase 2: MIDDLE-END (Optimizations)

### 2.1 Lambda IR Generation (`lambda/`)

**Input:** Typed AST (`Typedtree`)  
**Output:** Lambda IR

**Key Files:**
- `lambda.mli` - Lambda IR definition
- `translmod.ml` - Translate modules to Lambda
- `translcore.ml` - Translate core expressions to Lambda
- `translclass.ml` - Translate classes to Lambda
- `translobj.ml` - Translate objects to Lambda
- `translprim.ml` - Translate primitives to Lambda

**Pattern Matching Compilation:**
- `matching.ml` - Compile pattern matching to decision trees
- `switch.ml` - Optimize switches/pattern matches
- `tmc.ml` - Tail-modulo-cons optimization (tail recursion for list construction)

**What it does:**
- Convert typed AST to intermediate Lambda IR
- **Compile pattern matching** to efficient decision trees
- Handle exceptions, modules, objects
- Apply some early optimizations (TMC)
- Lambda is a higher-level IR still with functional constructs

**Lambda IR Features:**
- `Lvar` - Variables
- `Lconst` - Constants
- `Lapply` - Function application
- `Lfunction` - Function definition
- `Llet` / `Lletrec` - Let bindings
- `Lprim` - Primitive operations
- `Lswitch` - Compiled pattern matches
- `Lstaticraise` / `Lstaticcatch` - Exception handling
- `Lifthenelse`, `Lsequence`, `Lwhile`, `Lfor` - Control flow

**RAML Status:** ❌ TODO - This is Phase 3

---

### 2.2 Simplification Pass (`lambda/simplif.ml`)

**Input:** Lambda IR  
**Output:** Simplified Lambda IR

**What it does:**
- Constant folding
- Dead code elimination
- Let-binding simplification
- Inline small functions
- Simplify known applications

**RAML Status:** ❌ TODO - Phase 4 (optimizations)

---

### 2.3 Two Middle-End Paths

After Lambda, OCaml has two optimization pipelines:

#### Path A: Closure Conversion (Classic, default)

**Files:** `middle_end/closure/`

1. **Closure Conversion** (`closure.ml`)
   - Convert Lambda to Clambda
   - Closure creation and environment capture
   - Function inlining decisions

2. **Clambda** (`middle_end/clambda.mli`)
   - Lower-level IR than Lambda
   - Explicit closures and environments
   - Closer to C/assembly representation

**What it does:**
- Analyze free variables in functions
- Create explicit closure records
- Decide what to inline
- Prepare for code generation

#### Path B: Flambda (Advanced optimizer, `-O3`)

**Files:** `middle_end/flambda/`

A sophisticated optimizer with:
- Aggressive inlining
- Specialization (monomorphization)
- Unboxing optimizations
- Dead code elimination
- Common subexpression elimination

**Much more complex!** We'll skip this initially.

**RAML Decision:** Start with **Closure Conversion** (simpler, classic path)

---

## 🏭 Phase 3: BACKENDS

### 3.1 Bytecode Backend (`bytecomp/`)

**Input:** Lambda IR  
**Output:** Bytecode executable

**Key Files:**
- `bytegen.ml` - Generate bytecode from Lambda
- `instruct.mli` - Bytecode instruction set
- `emitcode.ml` - Emit bytecode to file
- `bytelink.ml` - Link bytecode modules

**Bytecode Instructions:**
- Stack-based virtual machine
- Simple instruction set (~100 opcodes)
- `PUSH`, `APPLY`, `RETURN`, `BRANCH`, `SWITCH`, etc.
- Portable across platforms

**What it does:**
- Compile Lambda directly to bytecode
- Stack allocation and management
- Generate executable bytecode file (`.cmo` → `.cma`)

**RAML Status:** ❌ TODO - Alternative backend option

---

### 3.2 Native Backend (`asmcomp/`)

**Input:** Clambda IR  
**Output:** Assembly code / object files

This is the **complex** part - multiple stages:

#### 3.2.1 CMM Generation (`cmmgen.ml`)

**Input:** Clambda  
**Output:** CMM (C-- intermediate form)

**What it does:**
- Convert Clambda to lower-level CMM
- Memory layout decisions
- Register allocation hints
- Generate memory management code (GC)

**CMM Features:**
- Explicit memory operations
- Load/store instructions
- Primitive operations (add, sub, call)
- Still platform-independent

#### 3.2.2 Selection (`selection.ml`)

**Input:** CMM  
**Output:** Mach (pseudo-assembly)

**What it does:**
- Instruction selection (pick actual instructions)
- Platform-specific optimizations
- Handle calling conventions

#### 3.2.3 Register Allocation

**Files:**
- `liveness.ml` - Liveness analysis
- `spill.ml` - Spilling decisions
- `split.ml` - Live range splitting
- `coloring.ml` - Graph coloring register allocation
- `reload.ml` - Reload spilled values

**What it does:**
- Analyze variable lifetimes
- Assign physical registers
- Spill to stack when needed
- **This is HARD!** Very complex algorithms

#### 3.2.4 Code Emission (`emit.mlp`)

**Input:** Mach with registers  
**Output:** Assembly code (`.s` file)

**What it does:**
- Generate actual assembly instructions
- Platform-specific (ARM, x86, etc.)
- Handle calling conventions
- Frame layout, prologue/epilogue

**Platform Files:**
Each target has its own implementation:
- `asmcomp/arm64/` - ARM64 backend
- `asmcomp/amd64/` - x86-64 backend
- `asmcomp/i386/` - x86-32 backend
- etc.

Each contains:
- `arch.ml` - Architecture description
- `selection.ml` - Instruction selection
- `emit.mlp` - Assembly emission
- `scheduling.ml` - Instruction scheduling

---

## 🎯 RAML Implementation Plan

Based on this analysis, here's our phased approach:

### ✅ Phase 1: Foundation (DONE!)
- [x] Types module - Type representations
- [x] TypeOperations - Basic type operations
- [x] Unification - Type unification & inference
- [x] Environment - Typing environment
- [x] TypeChecker - Expression type checking
- [x] CLI tool with JSON output

**Current State:** We have a working type checker!

---

### 📋 Phase 2: Lambda IR (NEXT)

**Goal:** TypedTree → Lambda translation

**Modules to create:**

1. **`Lambda.ml`** - Lambda IR definition
   ```ocaml
   type lambda =
     | Lvar of Identifier.t
     | Lconst of constant
     | Lapply of { fn : lambda; args : lambda list }
     | Lfunction of { params : Identifier.t list; body : lambda }
     | Llet of { id : Identifier.t; value : lambda; body : lambda }
     | Lifthenelse of lambda * lambda * lambda
     | Lsequence of lambda * lambda
     | Lprim of primitive * lambda list
   ```

2. **`TranslateCore.ml`** - Translate expressions
   - Convert TypedTree expressions to Lambda
   - Handle let-bindings, functions, application
   - Translate primitives

3. **`Matching.ml`** - Compile pattern matching
   - Build decision trees from patterns
   - Generate efficient switch statements
   - Handle exhaustiveness

**Deliverable:** `raml lambda --json <file>` outputs Lambda IR

**Estimated Effort:** ~1000 lines, 2-3 sessions

---

### 📋 Phase 3: Simplification

**Goal:** Optimize Lambda IR

**Module:**
- **`Simplify.ml`** - Lambda simplifications
  - Constant folding
  - Dead code elimination  
  - Inline small functions
  - Beta reduction

**Deliverable:** `raml lambda --optimized --json <file>`

**Estimated Effort:** ~500 lines, 1-2 sessions

---

### 📋 Phase 4: Bytecode Backend (Option A - Easier)

**Goal:** Lambda → Bytecode

**Modules:**

1. **`Bytecode.ml`** - Bytecode instruction set
2. **`BytecodeGen.ml`** - Generate bytecode from Lambda
3. **`BytecodeEmit.ml`** - Emit bytecode files
4. **`BytecodeInterp.ml`** - Simple bytecode interpreter (for testing!)

**Deliverable:** `raml compile --bytecode <file>` produces executable

**Estimated Effort:** ~800 lines, 2-3 sessions

---

### 📋 Phase 5: Native Backend (Option B - Harder but more interesting!)

**Goal:** Lambda → Native code

**Stages:**

#### 5.1 Closure Conversion
- **`Closure.ml`** - Convert Lambda to Clambda
- Analyze free variables
- Create explicit closures

#### 5.2 CMM Generation (Simplified)
- **`CMM.ml`** - Define C-- IR (simpler than OCaml's)
- **`CMMGen.ml`** - Lower Clambda to CMM
- Skip GC initially (manual memory management)

#### 5.3 Register Allocation (Simplified)
- **`Liveness.ml`** - Basic liveness analysis
- **`RegisterAlloc.ml`** - Simple greedy allocation (not graph coloring!)
- Use unlimited virtual registers, spill to stack as needed

#### 5.4 ARM64 Code Generation
- **`ARM64.ml`** - ARM64 instruction set
- **`ARM64Gen.ml`** - Emit ARM64 assembly
- **`ARM64Emit.ml`** - Write `.s` file
- Call `as` and `ld` to create executable

**Deliverable:** `raml compile --native <file>` produces ARM64 binary!

**Estimated Effort:** ~2000 lines, 4-6 sessions

---

## 📊 Complexity Analysis

### Easy (We've done these!)
- ✅ Parsing (using Syn)
- ✅ Type checking
- ✅ Type inference

### Medium
- Lambda IR generation
- Pattern match compilation
- Bytecode generation
- Closure conversion

### Hard
- Flambda optimizations
- Register allocation (graph coloring)
- Instruction scheduling
- GC integration

### Very Hard (Skip for v1)
- Whole-program optimization
- Advanced register allocation
- Multi-module compilation
- Separate compilation + linking

---

## 🎯 Recommended Path: "Vertical Slice"

**Goal:** End-to-end compilation as fast as possible

**Minimal viable compiler:**
1. ✅ Parse (Syn)
2. ✅ Type check (RAML TypeChecker)
3. ⏭️ Translate to simple Lambda IR
4. ⏭️ Generate ARM64 directly (skip most optimizations!)
5. ⏭️ Emit assembly, call `as`, create executable

**Start with the absolute minimum:**
- No pattern matching compilation (only simple patterns)
- No closures (only top-level functions)
- No GC (leak memory!)
- No optimizations
- Direct Lambda → ARM64 translation

**Why?** 
- Quick wins and momentum
- Test infrastructure in place
- See it working end-to-end
- Then iterate and improve

**Target:** Compile `let x = 42` to working ARM64 in ~1000 lines

---

## 📚 References

### OCaml Compiler Internals
- [Real World OCaml - Compiler Frontend](https://dev.realworldocaml.org/compiler-frontend.html)
- [OCaml Compiler Hacking Guide](https://github.com/ocaml/ocaml/blob/trunk/HACKING.adoc)
- [Compiling with Continuations, Continued (Flambda paper)](https://arxiv.org/abs/1702.06950)

### Type Checking
- Rémy, D. (1992). "Type checking records and variants in a natural extension of ML"
- Rémy, D. (1988). "Type Checking Records and Variants"

### Pattern Matching Compilation
- Maranget, L. (2008). "Compiling Pattern Matching to Good Decision Trees"
- Maranget, L. (2001). "Compiling Lazy Pattern Matching"

### Code Generation
- Appel, A. (1998). "Modern Compiler Implementation in ML"
- Cooper & Torczon (2011). "Engineering a Compiler"

---

## Summary Table: OCaml Compiler Passes

| Phase | Input | Output | Complexity | RAML Status |
|-------|-------|--------|------------|-------------|
| **Frontend** |
| Parsing | `.ml` | `Parsetree` | Easy | ✅ (via Syn) |
| Typing | `Parsetree` | `Typedtree` | Medium | ✅ Complete! |
| **Middle-End** |
| Lambda Gen | `Typedtree` | `Lambda` | Medium | ❌ Phase 3 |
| Simplify | `Lambda` | `Lambda` | Easy | ❌ Phase 4 |
| Closure | `Lambda` | `Clambda` | Medium | ❌ Phase 5 |
| Flambda | `Lambda` | `Flambda` | Hard | ⏸️ Skip v1 |
| **Backend** |
| CMM Gen | `Clambda` | `CMM` | Medium | ❌ Phase 6 |
| Selection | `CMM` | `Mach` | Medium | ❌ Phase 7 |
| Reg Alloc | `Mach` | `Mach` | Hard | ❌ Phase 8 |
| Emit | `Mach` | `.s` | Medium | ❌ Phase 9 |
| **Alternative** |
| Bytecode | `Lambda` | `Bytecode` | Easy | ⏸️ Optional |

---

## Next Session Goals

**Immediate (Phase 3):**
1. Create `Lambda.ml` - IR definition
2. Create `TranslateCore.ml` - TypedTree → Lambda
3. Compile simple expressions: `let x = 42`, `fun x -> x + 1`
4. Output Lambda IR as JSON

**Then (Vertical Slice):**
5. Skip to direct ARM64 emission!
6. Generate minimal assembly for constants and let-bindings
7. Call `as` and `ld` to create executable
8. **🎉 Celebrate:** We've built an end-to-end compiler!

Let's do this! 🚀
