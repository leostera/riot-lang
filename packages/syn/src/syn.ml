open Std
module Ceibo = Ceibo
module Error = Error
module Token = Token
module Keyword = Keyword
module Cursor = Cursor
module Lexer = Lexer
module SyntaxKind2 = Syntax_kind2
module RawToken = Raw_token
module Event = Event
module SyntaxTree = Syntax_tree
module Ast2 = Ast2
module Diagnostic = Diagnostic
module Parser2 = Parser2
module Parser = Parser2
module DiagnosticReporter = Diagnostic_reporter
module Deps = Deps

let tokenize = fun source -> Lexer.tokenize source

let source_slice = fun source -> IO.IoVec.IoSlice.from_string source |> Result.expect ~msg:"failed to create Syn source slice"

let parse_interface = fun source -> Parser2.parse_interface (source_slice source)

let parse_implementation = fun source -> Parser2.parse_implementation (source_slice source)

let parse = fun ~filename source ->
  match Path.extension filename with
  | Some ".mli" -> parse_interface source
  | _ -> parse_implementation source

let parse2 = fun ~filename source -> Parser2.parse ~filename source
