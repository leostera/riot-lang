# Andes OCaml Parser

This pratt parser for OCaml based on the design of the Rust parser.

It is split into 3 layers:

* a low-level lexer that turns `string` into a stream of tagged positions
* a lexer that turns tagged positions into typed tokens with positions
* a parser that takes a stream of tokens and returns a concrete syntax tree

## How does it work?

The Cursor module implements a lazy UTF-8 Char iterator over a string, and
lets you peek, advance, and skip characters.

The Lexer takes the Cursor, and uses it to create a stream of trees of tokens.



---

```ocaml
Andes OCaml TokenTrees (Multi-core):
  time per parsetree/:
    426.42 ms
  parsetrees over time/:
    11.73 1/s
Andes OCaml TokenTrees (Single-core):
  time per parsetree/:
    65.51 ms
  parsetrees over time/:
    15.27 1/s

(* inline next *)
Andes OCaml TokenTrees (Multi-core):
  time per parsetree/:
    368.54 ms
  parsetrees over time/:
    13.57 1/s
Andes OCaml TokenTrees (Single-core):
  time per parsetree/:
    63.77 ms
  parsetrees over time/:
    15.68 1/s

(* simplify cursor.next to short circuit on None *)


```
