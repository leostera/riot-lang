type t = string list

let report_unterminated_comment (token : AndesLexer.Lexer.token) =
  Printf.printf "Unterminated comment at %d\n%!" token.pos

let report_unterminated_docstring (token : AndesLexer.Lexer.token) =
  Printf.printf "Unterminated docstring at %d\n%!" token.pos
