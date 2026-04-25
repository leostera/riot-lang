open Std

(** Syn - OCaml lexer, streaming parser, diagnostics, and Ast2 views. *)

(** Red-green utility library used for source spans. *)
module Ceibo: module type of Ceibo

(** Structured parse error descriptions. *)
module Error: module type of Error

(** Token types produced by the lexer. *)
module Token: module type of Token

(** OCaml keyword definitions. *)
module Keyword: module type of Keyword

(** Low-level character cursor used by the lexer. *)
module Cursor: module type of Cursor

(** Lossless lexer. *)
module Lexer: module type of Lexer

(** Streaming parser lexical and grammar syntax kinds. *)
module SyntaxKind2: module type of Syntax_kind2

(** Source-backed raw token stream. *)
module RawToken: module type of Raw_token

(** Parser event stream. *)
module Event: module type of Event

(** Vector-backed lossless syntax tree. *)
module SyntaxTree: module type of Syntax_tree

(** Typed views over the lossless syntax tree. *)
module Ast2: module type of Ast2

(** Structured parser diagnostics. *)
module Diagnostic: module type of Diagnostic

(** Streaming parser. *)
module Parser2: module type of Parser2

(** Compatibility alias for the only parser path. *)
module Parser = Parser2

(** Diagnostic pretty-printer. *)
module DiagnosticReporter: module type of Diagnostic_reporter

(** Syntactic module dependency extraction. *)
module Deps: module type of Deps

(** Lex source code into token records with token-attached leading trivia. *)
val tokenize: string -> Token.t list

(** Parse .mli source with the streaming parser. *)
val parse_interface: string -> Parser2.parse_result

(** Parse .ml source with the streaming parser. *)
val parse_implementation: string -> Parser2.parse_result

(** Parse source with file-kind selection based on the filename extension. *)
val parse: filename:Std.Path.t -> string -> Parser2.parse_result

(** Parse an existing source slice with file-kind selection based on the
    filename extension. *)
val parse2: filename:Std.Path.t -> IO.IoVec.IoSlice.t -> Parser2.parse_result
