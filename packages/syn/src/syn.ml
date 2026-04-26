open Std

module Ceibo = Ceibo
module Error = Error
module Token = Token
module Keyword = Keyword
module Cursor = Cursor
module Lexer = Lexer
module SyntaxKind = Syntax_kind
module RawToken = Raw_token
module Event = Event
module SyntaxTree = Syntax_tree
module Ast = Ast
module Visitor = Visitor
module Diagnostic = Diagnostic
module Parser = Parser
module DiagnosticReporter = Diagnostic_reporter
module Deps = Deps

let tokenize = fun source -> Lexer.tokenize source

let parse_interface = Parser.parse_interface

let parse_implementation = Parser.parse_implementation

let parse = Parser.parse
