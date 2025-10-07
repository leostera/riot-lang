open Std

module Ceibo = Ceibo
module Token = Token
module Keyword = Keyword
module Cursor = Cursor
module Lexer = Lexer
module SyntaxKind = Syntax_kind
module Diagnostic = Diagnostic
module Parser = Parser

val tokenize : string -> Token.t list
(** Tokenize source code into tokens *)

val parse : string -> Parser.parse_result
(** Parse source code into a Ceibo green tree with diagnostics.
    Never fails - always returns a tree (possibly with ERROR nodes). *)
