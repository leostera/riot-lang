open Std

(** OCaml parser that produces Ceibo green trees *)

type parse_result = {
  tree : (Syntax_kind.t, string) Ceibo.Green.node;
  diagnostics : Diagnostic.t list;
}
(** Parse result always contains a tree (even if malformed) plus diagnostics.
    The parser never fails - it creates ERROR/MISSING nodes for problems. *)

val parse : source:string -> Token.t list -> parse_result
(** Parse OCaml source into a Ceibo green tree.
    
    Always returns a tree, even from malformed input. Any syntax errors
    are recorded as diagnostics while still producing a usable tree structure.
    
    @param source The original source code
    @param tokens List of tokens from the lexer
    @return Parse result with tree and diagnostics *)
