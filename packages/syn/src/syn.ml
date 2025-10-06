open Std
module Token = Token
module Cursor = Cursor
module Lexer = Lexer
module TokenTree = Token_tree

let tokenize source = Lexer.tokenize source
let parse_token_trees source = source |> tokenize |> TokenTree.of_tokens
