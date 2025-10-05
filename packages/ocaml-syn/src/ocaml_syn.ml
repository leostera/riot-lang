open Std
module Token = Token
module Cursor = Cursor
module Lexer = Lexer
module TokenTree = Token_tree
module Ast = Ast
module Parser = Parser

let tokenize source = Lexer.tokenize source
let parse_token_trees source = source |> tokenize |> TokenTree.of_tokens

let parse source =
  let tokens = tokenize source in
  let parser = Parser.create tokens in
  Parser.parse parser
