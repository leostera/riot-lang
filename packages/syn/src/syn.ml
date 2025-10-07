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

let parse source =
  let tokens = Lexer.tokenize source in
  Parser.parse ~source tokens
