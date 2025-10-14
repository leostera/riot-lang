open Std
module Ceibo = Ceibo
module Token = Token
module Keyword = Keyword
module Cursor = Cursor
module Lexer = Lexer
module SyntaxKind = Syntax_kind
module Diagnostic = Diagnostic
module Parser = Parser

let tokenize source = Lexer.tokenize source

let parse_interface source =
  let tokens = Lexer.tokenize source in
  Parser.parse_interface ~source tokens

let parse_implementation source =
  let tokens = Lexer.tokenize source in
  Parser.parse_implementation ~source tokens

let parse ~filename source =
  (* Decide based on file extension *)
  if String.ends_with ~suffix:".mli" filename then parse_interface source
  else parse_implementation source
