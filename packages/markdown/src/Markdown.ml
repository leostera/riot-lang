open Std

type t = (Syntax_kind.t, string) Ceibo.Green.node

let parse input =
  let tokens = Lexer.tokenize input in
  let tree = Parser.parse ~source:input tokens in
  tree

let compile tree =
  Compiler.compile "" tree
