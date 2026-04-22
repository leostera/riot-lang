open Std
open Std.Collections

type file_kind =
[
  | `Implementation
  | `Interface
]
type parse_result = {
  source: IO.IoVec.IoSlice.t;
  kind: file_kind;
  tokens: Raw_token.stream;
  tree: Syntax_tree.t;
  diagnostics: Diagnostic.t Vector.t;
}
val parse_implementation: IO.IoVec.IoSlice.t -> parse_result

val parse_interface: IO.IoVec.IoSlice.t -> parse_result

val parse: filename:Std.Path.t -> IO.IoVec.IoSlice.t -> parse_result
