open Parser
open Lexer

let run () =
  let tokens = tokenize "input" in
  List.length tokens
