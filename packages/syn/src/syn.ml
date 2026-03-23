open Std
module Ceibo = Ceibo
module Error = Error
module Token = Token
module Keyword = Keyword
module Cursor = Cursor
module Lexer = Lexer
module SyntaxKind = Syntax_kind
module Diagnostic = Diagnostic
module Parser = Parser
module DiagnosticReporter = Diagnostic_reporter
module Cst = Cst
module Traversal = Traversal
module Visit = Visit
module CstBuilder = Cst_builder
module CstJson = Cst_json

let tokenize source = Lexer.tokenize source

let attach_cst ~kind result =
  if List.length result.Parser.diagnostics = 0 then
    match CstBuilder.create_from_ceibo ~kind result.tree with
    | Ok cst -> { result with Parser.cst = Some cst }
    | Error _ -> { result with Parser.cst = None }
  else
    { result with Parser.cst = None }

let parse_interface source =
  let tokens = Lexer.tokenize source in
  Parser.parse_interface ~source tokens |> attach_cst ~kind:`Interface

let parse_implementation source =
  let tokens = Lexer.tokenize source in
  Parser.parse_implementation ~source tokens |> attach_cst ~kind:`Implementation

let parse ~filename source =
  match Path.extension filename with
  | Some ".mli" -> parse_interface source
  | _ -> parse_implementation source
