# RAML Vertical Slice Plan

## Goal: End-to-End Compilation of `let x = 42`

Get the simplest possible program working through the entire pipeline:

```ocaml
let x = 42
```

**Target output:** ARM64 assembly that defines and returns 42.

## Vertical Slice Layers

### 1. Parse (Using Syn) ✅
```
"let x = 42" → Syn CST → Extract structure
```

**Status:** Syn exists, we just need to consume it

### 2. Type Check → Typed Tree (PRIORITY)
```
Syn CST → Typed Tree
```

**Need to implement:**
- [ ] `TypedTree` - Typed AST representation
- [ ] `Btype` - Basic type operations (repr, level management)
- [ ] `Ctype` - Unification engine
- [ ] `Env` - Typing environment (value bindings)
- [ ] `TypeCore` - Expression type checker
  - [ ] Type check literals (int)
  - [ ] Type check identifiers (variables)
  - [ ] Type check let bindings

**Test:** `let x = 42` → `TypedTree.LetBinding { ident; type=int; expr }`

### 3. Translate to Lambda IR
```
Typed Tree → Lambda IR
```

**Need to implement:**
- [ ] `Lambda` - Lambda IR AST
  - `Lvar` - Variable reference
  - `Lconst` - Constant (int, string, etc.)
  - `Llet` - Let binding
- [ ] `TranslCore` - Typed Tree → Lambda translation

**Test:** `TypedTree.LetBinding` → `Llet(x, Lconst(42), Lvar(x))`

### 4. Simplify Lambda
```
Lambda IR → Simplified Lambda IR
```

**Need to implement:**
- [ ] `Simplif` - Lambda simplification pass
  - Constant propagation
  - Dead code elimination
  - Beta reduction

**Test:** No changes for `let x = 42` (already simple)

### 5. Generate CMM (C--)
```
Lambda IR → CMM (low-level IR)
```

**Need to implement:**
- [ ] `Cmm` - CMM AST
  - `Cconst_int` - Integer constant
  - `Cvar` - Variable
  - `Clet` - Let binding
- [ ] `CmmGen` - Lambda → CMM translation

**Test:** `Llet(x, Lconst(42), Lvar(x))` → CMM representation

### 6. Generate ARM64 Assembly
```
CMM → ARM64 assembly
```

**Need to implement:**
- [ ] `Arm64` - ARM64 instruction set
- [ ] `Arm64Emit` - CMM → ARM64 translation
- [ ] Register allocation (simple for now)
- [ ] Stack frame management

**Test:** CMM → ARM64 assembly that returns 42

## Implementation Order (Vertical Slice First)

### Week 1: Type Checking (CURRENT)
- [x] Foundation (Identifier, Types, ModulePath)
- [ ] TypedTree structure
- [ ] Btype operations
- [ ] Ctype unification (minimal - just int type)
- [ ] Env (minimal - just value bindings)
- [ ] TypeCore (minimal - literals + let)

**Milestone:** Type check `let x = 42` → TypedTree

### Week 2: Lambda IR
- [ ] Lambda AST (minimal subset)
- [ ] TranslCore (literals + let → Lambda)
- [ ] Basic simplification

**Milestone:** `let x = 42` → Lambda IR

### Week 3: CMM + ARM64
- [ ] CMM AST
- [ ] CmmGen (Lambda → CMM)
- [ ] ARM64 instructions
- [ ] ARM64 emission

**Milestone:** `let x = 42` → Working ARM64 binary

### Week 4: Expand Vertical Slice
- [ ] Add function calls: `let f x = x + 1`
- [ ] Add if/then/else
- [ ] Add pattern matching (simple)

## Minimal Type System for Vertical Slice

Start with absolute minimum:

```ocaml
type type_expr =
  | Variable of string option
  | Constructor of ModulePath.t * type_expr list
  (* Later: Arrow, Tuple, etc. *)
```

**Predef types:**
- `int` - integers only for now
- `unit` - for later

**No polymorphism yet!** Just monomorphic types.

## Minimal Environment

```ocaml
type env = {
  values: (Identifier.t, type_expr) Collections.HashMap.t;
  (* Later: types, modules, etc. *)
}
```

## Testing Strategy

### Unit Tests (per module)
- `test_btype.ml` - Type representation tests
- `test_ctype.ml` - Unification tests
- `test_env.ml` - Environment tests
- `test_typeCore.ml` - Type checking tests

### Integration Tests (end-to-end)
- `test_vertical_slice.ml` - Full pipeline test

```ocaml
let test_vertical_slice () =
  let source = "let x = 42" in
  
  (* Parse *)
  let cst = Syn.Parser.parse source |> Result.unwrap in
  
  (* Type check *)
  let ctx = Types.create_context () in
  let typed_tree, ctx = TypeCore.type_structure ~ctx cst in
  
  (* Translate to Lambda *)
  let lambda_ir = TranslCore.translate typed_tree in
  
  (* Simplify *)
  let lambda_ir = Simplif.simplify lambda_ir in
  
  (* Generate CMM *)
  let cmm = CmmGen.generate lambda_ir in
  
  (* Generate ARM64 *)
  let asm = Arm64Emit.emit cmm in
  
  (* Verify *)
  assert (String.contains asm "mov")
```

## Success Criteria

**Phase 1 (Type Check):**
```bash
$ tusk run raml -- typecheck tests/fixtures/simple/0001_int_literal.ml
Type: int
```

**Phase 2 (Lambda):**
```bash
$ tusk run raml -- lambda tests/fixtures/simple/0001_int_literal.ml
(Lconst (Const_base (Const_int 42)))
```

**Phase 3 (ARM64):**
```bash
$ tusk run raml -- compile tests/fixtures/simple/0001_int_literal.ml -o test.o
$ file test.o
test.o: Mach-O 64-bit object arm64
```

**Phase 4 (Execute):**
```bash
$ tusk run raml -- run tests/fixtures/simple/0001_int_literal.ml
42
```

## Key Decisions for Vertical Slice

1. **No polymorphism yet** - Just monomorphic types (int, unit)
2. **No pattern matching yet** - Just simple let bindings
3. **No functions yet** - Just constants and variables
4. **No modules yet** - Single file compilation
5. **Minimal optimizations** - Just constant folding

Focus: **Working end-to-end ASAP**, then expand horizontally.

## File Structure for Vertical Slice

```
packages/raml/src/
├── typechecker/
│   ├── identifier.ml         ✅
│   ├── modulePath.ml         ✅
│   ├── types.ml              ✅
│   ├── typedTree.ml          ⏳ Next
│   ├── btype.ml              ⏳ Next
│   ├── ctype.ml              ⏳ Next
│   ├── env.ml                ⏳ Next
│   └── typeCore.ml           ⏳ Next
│
├── lambda/
│   ├── lambda.ml             🔜 Week 2
│   └── translCore.ml         🔜 Week 2
│
├── simplify/
│   └── simplif.ml            🔜 Week 2
│
├── cmm/
│   ├── cmm.ml                🔜 Week 3
│   └── cmmGen.ml             🔜 Week 3
│
└── arm64/
    ├── arm64.ml              🔜 Week 3
    └── arm64Emit.ml          🔜 Week 3
```

## Current Status

**Completed:** Foundation (255 lines)
**Next:** TypedTree + Btype (target: 200 lines)
**Then:** Ctype + Env + TypeCore (target: 500 lines)

**Total vertical slice estimate:** ~1500 lines for end-to-end
