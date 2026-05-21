use Token

/// Minimal lexer state for the first hardcoded bootstrap lexer slices.
type LexerState = LexerState(String)

fn state(input: String) -> LexerState {
  LexerState(input)
}

fn input_text(state: LexerState) -> String {
  match state {
    LexerState(input) -> input
  }
}

fn lex_empty(_input: String) -> List<Token.Token> {
  [Token.Eof]
}

fn identifier_or_keyword(input: String) -> Token.Token {
  if input == "fn" {
    Token.Fn
  } else {
    if input == "let" {
      Token.Let
    } else {
      Token.Identifier(input)
    }
  }
}

fn lex(input: String) -> List<Token.Token> {
  if input == "" {
    lex_empty(input)
  } else {
    [identifier_or_keyword(input), Token.Eof]
  }
}
