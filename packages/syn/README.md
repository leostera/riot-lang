# ocaml-syn

A clean OCaml parser producing a traversable AST.

## Purpose

Parse OCaml source code into a simple, canonical AST that's easy to:
- Traverse
- Pattern match
- Transform
- Query

**Not** for compilation - for tooling!

## Architecture

```
Source Code
    ↓
  Lexer → Token stream
    ↓
  TokenTree → Hierarchical token structure
    ↓
  Parser → Clean AST
```

## Features

### 1. Tokens
Full OCaml lexer with all keywords, operators, literals.

### 2. TokenTrees
Hierarchical structure based on delimiters (parens, braces, brackets, begin/end, etc).

**Key insight**: TokenTrees give you structure without parsing semantics.
Perfect for macros and code mods (see separate packages).

### 3. AST
Clean, canonical representation:
- Desugared (one way to represent each concept)
- Easy to traverse
- Easy to pattern match
- No syntactic noise

**Example**:
```ocaml
(* Source code: *)
let add x y = x + y

(* AST: *)
Let(Nonrecursive, 
    PatVar "add",
    Fun(PatVar "x", Fun(PatVar "y", 
        Infix("+", Var "x", Var "y"))))
```

All functions are curried, everything desugared to core forms.

## API

```ocaml
(* Tokenize *)
val Lexer.tokenize : string -> Token.t list

(* Build token trees *)
val TokenTree.of_tokens : Token.t list -> TokenTree.t list

(* Parse to AST *)
val Parser.parse : Token.t list -> (Ast.structure, error) result

(* Convenience *)
val parse : string -> (Ast.structure, error) result
```

## Desugaring Rules

ocaml-syn normalizes syntax to canonical forms:

| Syntax | Desugars To |
|--------|-------------|
| `function \| p -> e` | `fun _x -> match _x with \| p -> e` |
| `let f x y = e` | `let f = fun x -> fun y -> e` |
| `[1; 2; 3]` | `1 :: (2 :: (3 :: []))` |
| `(e1; e2; e3)` | `let _ = e1 in let _ = e2 in e3` |
| `if c then t else e` | `match c with \| true -> t \| false -> e` |

This makes the AST much easier to work with - you only handle core forms.

## Related Packages

- **ocaml-macros** - Macro system operating on TokenTrees
- **ocaml-fmt** - Code formatter using the AST
- **ocaml-refactor** - Code transformation tools
- **ocaml-analyzer** - Semantic analysis and queries

## Example Usage

```ocaml
open Std

let source = {|
  let add x y = x + y
  let result = add 1 2
|}

(* Parse *)
match Ocaml_syn.parse source with
| Error err -> println "Parse error!"
| Ok ast ->
    (* Traverse AST *)
    let rec find_lets = function
      | Ast.LetItem (_, bindings) :: rest ->
          List.length bindings + find_lets rest
      | _ :: rest -> find_lets rest
      | [] -> 0
    in
    println "Found %d let bindings" (find_lets ast)
```

## Design Philosophy

**Simple over Complete**:
- Start with core OCaml
- Add features incrementally
- Keep AST minimal and clean

**Canonical over Faithful**:
- Desugar to core forms
- One representation per concept
- Easier to work with

**Traversable over Detailed**:
- Easy pattern matching
- Simple recursion schemes
- Minimal noise

## Non-Goals

- Not for compilation (use OCaml's compiler-libs for that)
- Not byte-for-byte source preservation (use CST for that)
- Not error recovery (fail fast, report clearly)
- Not compatible with OCaml's Parsetree (intentionally different)

## Status

🚧 Early development - implementing core features

Implemented:
- ✅ Lexer (complete)
- ✅ TokenTrees (complete)
- 🚧 Parser (core expressions done, patterns/types/modules in progress)

See DESIGN.md for detailed design decisions.
