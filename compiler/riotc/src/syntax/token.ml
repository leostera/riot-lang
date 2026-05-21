/// Tokens recognized by the first riotc lexer slices.
type Token =
  | Identifier(String)
  | IntLiteral(i64)
  | StringLiteral(String)
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Colon
  | Arrow
  | Equal
  | Fn
  | Let
  | Eof

fn render(token: Token) -> String {
  match token {
    Identifier(name) -> string_concat("identifier:", name),
    IntLiteral(_) -> "int",
    StringLiteral(_) -> "string",
    LeftParen -> "left-paren",
    RightParen -> "right-paren",
    LeftBrace -> "left-brace",
    RightBrace -> "right-brace",
    Comma -> "comma",
    Colon -> "colon",
    Arrow -> "arrow",
    Equal -> "equal",
    Fn -> "fn",
    Let -> "let",
    Eof -> "eof"
  }
}
