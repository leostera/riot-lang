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

fn lex(input: String) -> List<Token.Token> {
  if input == "" {
    lex_empty(input)
  } else {
    [Token.Eof]
  }
}
