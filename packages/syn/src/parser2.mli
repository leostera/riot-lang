open Std
open Std.Collections

type file_kind =
  [
    | `Implementation
    | `Interface
  ]

type parse_result = {
  source: string;
  kind: file_kind;
  tokens: Raw_token.stream;
  events: Event.Buffer.t;
  tree: Syntax_tree.t;
  diagnostics: Diagnostic.t Vector.t;
}

val parse_implementation: string -> parse_result

val parse_interface: string -> parse_result

val parse: filename:Std.Path.t -> string -> parse_result

