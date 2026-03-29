# Ceibo - Generic Red-Green Syntax Trees

A generic, language-agnostic implementation of red-green syntax trees for building lossless, incremental parsers.

## Overview

Ceibo implements the red-green syntax tree architecture popularized by:
- Roslyn (C# compiler)
- Swift's libsyntax
- Rust Analyzer's Rowan

### Key Properties

1. **Lossless** - Every byte of input is represented in the tree
2. **Immutable** - Nodes can be safely shared across trees
3. **Efficient** - Arena allocation, structural sharing, lazy construction
4. **Incremental** - Support for efficient re-parsing on edits

## Architecture

### Green Tree (Base Layer)

The green tree represents abstract syntax with **no position information**:

```ocaml
type green_trivia = {
  kind: int;        (* Trivia kind ID *)
  text: string;     (* Trivia text *)
  width: int;       (* Trivia byte width *)
}

type green_token = {
  kind: int;                (* Token kind ID *)
  text: string;             (* Token body text *)
  width: int;               (* Token body width *)
  leading_trivia: green_trivia list;
}

type green_node = {
  kind: int;        (* Node kind ID *)
  width: int;       (* Cached total width *)
  children: green_element array;
}

and green_element =
  | Token of green_token
  | Node of green_node
```

**Properties:**
- Immutable and position-independent
- Can be shared across multiple parents
- Width is sum of children widths (cached)
- Arena-allocated for efficiency

### Red Tree (View Layer)

The red tree is a **lazy view** over the green tree with position information:

```ocaml
type syntax_node = {
  green: green_node;
  parent: syntax_node option;
  offset: int;      (* Absolute offset from start *)
}

type syntax_token = {
  green: green_token;
  parent: syntax_node option;
  offset: int;
}
```

**Properties:**
- Fabricated on-demand (lazy)
- Provides parent links for traversal
- Provides absolute positions via offset + width
- Garbage collected when no longer referenced

## Core Operations

### Construction (Green Tree)

```ocaml
(* Build green nodes bottom-up *)
let token = Green.make_token ~leading_trivia:[] ~kind:INT ~text:"42" ~width:2
let node = GreenNode.make ~kind:LIT_EXPR ~children:[Token token]
```

### Traversal (Red Tree)

```ocaml
(* Get red view with positions *)
let root = SyntaxNode.new_root green_tree
let child = SyntaxNode.child root 0
let offset = SyntaxNode.offset child
let range = SyntaxNode.text_range child
```

### Modification

```ocaml
(* Modification creates new green tree with structural sharing *)
let new_green = GreenNode.replace_child old_green 2 new_child
let new_red = SyntaxNode.new_root new_green
```

## Memory Architecture

### Structural Sharing

```
Before:    let x = 1 + 1
After:     let x = 2 + 1

Green Tree (shared):
           BIN_EXPR
          /    |    \
         /     +     \
        1 ←---------→ 1  (same token shared)

After edit:
           BIN_EXPR
          /    |    \
         2     +     1  (reused + and 1)
```

## Use Cases

### 1. Lossless Formatting

Preserve all whitespace and comments:

```ocaml
(* Original source *)
"let x =  1  (* comment *)"

(* Trivia is attached to the next token *)
[
  Token(LET, "let", leading_trivia = []);
  Token(IDENT, "x", leading_trivia = [Trivia(SPACE, " ")]);
  Token(EQ, "=", leading_trivia = [Trivia(SPACE, " ")]);
  Token(INT, "1", leading_trivia = [Trivia(SPACES, "  ")]);
  EOF(leading_trivia = [Trivia(SPACES, "  "); Trivia(COMMENT, "(* comment *)")]);
]
```

### 2. Incremental Parsing

Re-parse only changed regions:

```ocaml
(* Edit: change "1" to "42" at offset 8 *)
let edited = incremental_parse
  ~old_tree
  ~old_source
  ~new_source
  ~edits:[{offset=8; len=1; text="42"}]

(* Reuses unchanged subtrees *)
```

### 3. Error Recovery

Parse incomplete/malformed code:

```ocaml
(* Malformed: missing RHS *)
"let x = "

(* Tree still produced with error node *)
LET_BINDING [
  Token(LET, "let");
  Token(IDENT, "x");
  Token(EQ, "=");
  ERROR_NODE [];  (* Missing expression *)
]
```

## Design Principles

1. **Immutability** - All nodes are immutable; edits create new trees
2. **Lazy Red Layer** - Red nodes fabricated on-demand, not stored
3. **Width Caching** - Cache total width on green nodes for O(1) positioning
4. **No Parent in Green** - Green nodes have no parent pointers (enables sharing)
5. **Structural Sharing** - Unchanged subtrees shared across versions

## References

- [Roslyn - Immutable Syntax Trees](https://github.com/KirillOsenkov/Bliki/wiki/Roslyn-Immutable-Trees)
- [Swift libsyntax](https://github.com/apple/swift/tree/main/lib/Syntax)
- [Rust Analyzer Rowan](https://github.com/rust-analyzer/rowan)
- [Red-Green Trees Overview](https://pling.jondgoodwin.com/post/red-green-tree/)
