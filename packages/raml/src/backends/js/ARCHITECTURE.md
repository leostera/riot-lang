# JavaScript Backend Architecture

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
│   Jambda    │  ← JavaScript-aware IR (J IR equivalent)
└──────┬──────┘    • Uncurrying optimization
       │           • Runtime representation decisions
       │           • JS primitive operations
       ▼
┌─────────────┐
│   JsTree    │  ← JavaScript AST (1:1 with JS syntax)
└──────┬──────┘    • Statement vs expression distinction
       │           • ES6 modules or CommonJS
       │           • Source map generation
       ▼
┌─────────────┐
│ JavaScript  │  ← Final string output
└─────────────┘
```

## Layer Responsibilities

### Lambda IR → Jambda

**File:** `lambda_to_jambda.ml`

**Transformations:**
- **Uncurrying detection**: Detect when `f(a)(b)(c)` can become `f(a,b,c)`
- **Runtime representation**: Decide how OCaml values map to JS
  - Variants → `{ TAG: n, _0: x, _1: y }`
  - Records → `{ field1: x, field2: y }`
  - Tuples → `[x, y, z]`
  - Arrays → `[x, y, z]`
- **Primitive mapping**: Lambda primitives → Jambda primitives
  - `Pint_add` → `Jadd` (direct JS `+`)
  - `Pfield 0` → `Jobject_get "_0"`
- **Currying strategy**: 
  - Known arity → uncurried application
  - Unknown arity → explicit curry helper

**Example:**
```ocaml
(* Lambda IR *)
Lapply {
  func = Lapply { func = f; args = [a] };
  args = [b]
}

(* Jambda - detected uncurrying *)
Japply_uncurried {
  func = f;
  args = [a; b]  (* Both args together! *)
}
```

### Jambda → JsTree

**File:** `jambda_to_jstree.ml`

**Transformations:**
- **Expression → Statement**: Jambda is expression-based, JS has statements
- **Control flow**: 
  - `Jifthenelse` → `JsIf` statement or `JsCond` expression
  - `Jswitch` → `JsSwitch` statement
- **Variable bindings**:
  - `Jlet` → `JsVarDecl(JsConst, ...)`
  - `Jletrec` → Multiple `JsVarDecl(JsLet, ...)` + assignments
- **Module structure**:
  - Jambda exports → `JsExport`
  - Jambda imports → `JsImport`

**Example:**
```ocaml
(* Jambda *)
Jlet {
  id = x;
  value = Jconst (Jconst_int 42);
  body = Japply_uncurried { func = f; args = [Jvar x] }
}

(* JsTree *)
JsSeq [
  JsVarDecl(JsConst, "x", Some (JsLit (JsNum 42.0)));
  JsCall { func = JsId "f"; args = [JsId "x"] }
]
```

### JsTree → JavaScript

**File:** `jstree_to_js.ml`

**Transformations:**
- **Pretty printing**: Convert AST to string
- **Formatting**: Indentation, newlines, comments
- **Source maps**: Optional `.js.map` generation
- **Module format**:
  - ES6: `import`/`export`
  - CommonJS: `require`/`module.exports`
  - IIFE: `(function() { ... })()`

**Example:**
```ocaml
(* JsTree *)
JsVarDecl(JsConst, "x", Some (JsNum 42.0))

(* JavaScript *)
"const x = 42;"
```

## Key Design Decisions

### 1. Why Jambda (not Lambda → JS directly)?

- **Separation of concerns**: Lambda is shared with native backends
- **JS-specific optimizations**: Uncurrying, representation choices
- **Easier to maintain**: Clear boundary between functional and imperative

### 2. Why JsTree (not Jambda → JS directly)?

- **Source maps**: Need precise location tracking
- **JS optimizations**: Constant folding, dead code elimination
- **Multiple output formats**: ES6, CommonJS, IIFE from same AST

### 3. Runtime Representation Choices

| OCaml Type | Jambda Tag | JsTree / JavaScript |
|------------|------------|---------------------|
| `int` | `TagInt` | `number` |
| `float` | `TagFloat` | `number` |
| `string` | `TagString` | `string` |
| `bool` | `TagBool` | `0`/`1` (or `boolean`) |
| `unit` | `TagUnit` | `undefined` |
| `Some x` | `TagVariant 0` | `{ TAG: 0, _0: x }` |
| `None` | `TagVariant 1` | `{ TAG: 1 }` |
| `{ x; y }` | `TagRecord` | `{ x: ..., y: ... }` |
| `[|a; b|]` | `TagArray` | `[a, b]` |
| `(a, b)` | `TagTuple` | `[a, b]` |

### 4. Currying Strategy

**Auto-uncurrying when safe:**
```ocaml
(* OCaml *)
let f x y = x + y
let result = f 1 2

(* Jambda - detected 2-arg function *)
Jfunction { arity = 2; params = [x; y]; ... }
Japply_uncurried { func = f; args = [1; 2] }

(* JavaScript *)
function f(x, y) { return x + y; }
const result = f(1, 2);
```

**Explicit currying when needed:**
```ocaml
(* OCaml *)
let add1 = f 1  (* Partial application *)

(* Jambda - curry helper needed *)
Japply_curried { func = f; arg = 1 }

(* JavaScript *)
const add1 = _curry2(f, 1);  // Returns closure
```

## File Organization

```
backend/js/
  ├── jambda.ml/mli           # Jambda IR definition
  ├── jstree.ml/mli           # JavaScript AST definition
  ├── lambda_to_jambda.ml/mli # Lambda → Jambda translation
  ├── jambda_to_jstree.ml/mli # Jambda → JsTree translation
  ├── jstree_to_js.ml/mli     # JsTree → JavaScript string
  ├── js_runtime.ml/mli       # Runtime library (curry helpers, etc.)
  ├── compile.ml/mli          # Orchestration
  └── ARCHITECTURE.md         # This file
```

## Runtime Library

The JavaScript backend requires a small runtime library for:
- Currying helpers: `_curry2`, `_curry3`, etc.
- Variant construction: `_variant(tag, ...args)`
- Pattern matching helpers
- List operations (if not using arrays)

This runtime is either:
- **Inlined** into generated code (small programs)
- **External module** (large programs, shared across modules)

## Future Optimizations

1. **Dead code elimination**: Remove unused exports
2. **Inlining**: Inline small functions
3. **Constant folding**: Compute constants at compile time
4. **Tail call optimization**: Convert tail recursion to loops
5. **Flattening**: Remove intermediate closures
6. **Specialized primitives**: Use TypedArrays for arrays, etc.
