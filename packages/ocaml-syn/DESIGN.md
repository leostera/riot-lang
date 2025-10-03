# OCaml-Syn Design Philosophy

## Purpose

This parser is **NOT** for compilation. It's for:
1. **Code formatting** - Preserve and pretty-print source
2. **Code analysis** - Query structure, find patterns, build dependency graphs
3. **Code transformation** - Macros, refactoring, code generation

## Architecture

### Three Representations

#### 1. Concrete Syntax Tree (CST)
**Purpose**: Lossless representation for formatting

```ocaml
type cst_node = {
  kind: cst_kind;
  span: span;           (* exact source location *)
  leading: trivia list; (* whitespace, comments before *)
  trailing: trivia list; (* whitespace, comments after *)
}

type trivia =
  | Whitespace of string
  | LineComment of string
  | BlockComment of string
  | Newline
```

**Properties**:
- Round-trips perfectly: `parse(format(parse(source))) = source`
- Preserves all comments, whitespace, parentheses
- Can format without changing unmodified parts
- Every syntactic detail is present

#### 2. Abstract Syntax Tree (AST)
**Purpose**: Clean representation for analysis and queries

```ocaml
type expr =
  | Var of string
  | Const of constant
  | App of expr * expr           (* No list - simpler! *)
  | Fun of pattern * expr        (* Single param - cleaner! *)
  | Let of pattern * expr * expr
  | ...

(* No location info unless needed *)
(* No comments *)
(* No syntactic sugar - desugar everything *)
```

**Properties**:
- Minimal - one way to represent each concept
- Easy to pattern match
- Easy to query
- Desugared (no `function`, just `fun` + `match`)

#### 3. High-Level IR (for transformations)
**Purpose**: Easy to construct, hard to make invalid

```ocaml
(* Builder API *)
let example =
  let_ "x" (int 42) @@
  let_ "y" (int 1) @@
  app (var "add") [var "x"; var "y"]

(* Type-safe construction *)
(* Automatic hygiene (fresh names) *)
(* Easy splicing *)
```

### Desugaring Strategy

The AST should desugar syntax to canonical forms:

**Before (OCaml syntax - many ways to say the same thing)**:
- `function | p1 -> e1 | p2 -> e2`
- `fun x -> match x with | p1 -> e1 | p2 -> e2`

**After (AST - one canonical form)**:
```ocaml
Fun(Var "_x", Match(Var "_x", [
  (p1, e1);
  (p2, e2);
]))
```

**More examples**:
- `let f x y = body` → `let f = fun x -> fun y -> body`
- `[1; 2; 3]` → `1 :: (2 :: (3 :: []))`
- `(e1; e2; e3)` → `let _ = e1 in let _ = e2 in e3`
- `if c then t else e` → `match c with | true -> t | false -> e`

### Why This Matters

**For Formatting**:
- CST preserves user's original formatting
- Can do minimal changes (only reformat modified parts)
- Comments stay attached to the right nodes

**For Analysis**:
- AST is simple to traverse
- No need to handle 5 different ways to write the same thing
- Queries are straightforward: "find all function applications"

**For Transformations**:
- IR makes it easy to generate valid code
- No need to worry about syntactic edge cases
- Can focus on logic, not syntax

## Comparison

### OCaml's Parsetree (Compiler)
```ocaml
type expression =
  | Pexp_ident of Longident.t loc
  | Pexp_constant of constant
  | Pexp_let of rec_flag * value_binding list * expression
  | Pexp_function of case list
  | Pexp_fun of arg_label * expression option * pattern * expression
  | Pexp_apply of expression * (arg_label * expression) list
  | (* 40+ more constructors *)
```

**Problems**:
- Too many constructors (60+)
- No separation of syntax vs semantics
- Comments in separate structure
- Hard to transform correctly

### Our AST (Tools)
```ocaml
type expr =
  | Var of string
  | Const of constant
  | App of expr * expr
  | Fun of pattern * expr
  | Let of rec_flag * pattern * expr * expr
  | Match of expr * (pattern * expr) list
  (* ~15 core constructors *)
```

**Benefits**:
- Minimal - easy to handle all cases
- Semantic - represents meaning, not syntax
- Transformation-friendly
- Query-friendly

## Implementation Strategy

### Phase 1: Lexer + TokenTrees (DONE ✓)
- Full token stream with positions
- Hierarchical structure via delimiters

### Phase 2: CST Parser (NEXT)
- Build CST from TokenTrees
- Attach trivia (comments, whitespace) to nodes
- Preserve everything for round-tripping

### Phase 3: AST Lowering
- CST → AST transformation
- Desugar all syntactic forms
- Clean, canonical representation

### Phase 4: Formatter
- AST → CST (with formatting rules)
- CST → Text (serialize)

### Phase 5: Analysis Tools
- Query API over AST
- Scope analysis
- Dependency tracking

### Phase 6: Transformation API
- High-level IR builders
- Macro system
- Refactoring tools

## Examples

### Query: "Find all function calls to 'Fs.read'"

**With CST** (hard):
```ocaml
(* Have to handle:
   - Pexp_apply with Pexp_ident "Fs.read"
   - But also Pexp_ident with Lident vs Ldot
   - And arg_label variants
   - And possible Pexp_fun wrapping
   - etc.
*)
```

**With our AST** (easy):
```ocaml
let rec find_calls acc = function
  | App (Var "Fs.read", arg) -> arg :: acc
  | App (f, x) -> find_calls (find_calls acc f) x
  | Let (_, _, e1, e2) -> find_calls (find_calls acc e1) e2
  | Match (e, cases) -> 
      List.fold_left (fun acc (_, e) -> find_calls acc e) 
        (find_calls acc e) cases
  | Fun (_, e) -> find_calls acc e
  | _ -> acc
```

### Transform: "Replace 'print' with 'println'"

**With our IR**:
```ocaml
let transform = function
  | App (Var "print", arg) -> app (var "println") arg
  | other -> other
```

Clean and obvious!

## Decision: Build CST First

Let's build the CST properly, THEN extract AST from it.

CST needs to track:
- Every token with exact position
- All whitespace and comments (trivia)
- Parentheses (even redundant ones)
- Original formatting

This will make the formatter work perfectly, and we can always
extract a clean AST for analysis later.
