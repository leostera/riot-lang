type source_char =
  | F
  | N
  | M
  | A
  | I
  | L
  | E
  | T
  | S
  | W
  | R
  | D4
  | D2
  | Space
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Equal

type scan_state =
  | Start
  | InIdent(String)
  | InNumber(i64)

type lexeme =
  | Word(String)
  | Number(i64)
  | Symbol(String)

type summary = { functions: i64, bindings: i64, identifiers: i64, literals: i64, symbols: i64 }

fn is_letter(ch: source_char) -> bool {
  match ch {
    F -> true,
    N -> true,
    M -> true,
    A -> true,
    I -> true,
    L -> true,
    E -> true,
    T -> true,
    S -> true,
    W -> true,
    R -> true,
    _ -> false
  }
}

fn is_digit(ch: source_char) -> bool {
  match ch {
    D4 -> true,
    D2 -> true,
    _ -> false
  }
}

fn append_letter(word: String, ch: source_char) -> String {
  match ch {
    F -> string_concat(word, "f"),
    N -> string_concat(word, "n"),
    M -> string_concat(word, "m"),
    A -> string_concat(word, "a"),
    I -> string_concat(word, "i"),
    L -> string_concat(word, "l"),
    E -> string_concat(word, "e"),
    T -> string_concat(word, "t"),
    S -> string_concat(word, "s"),
    W -> string_concat(word, "w"),
    R -> string_concat(word, "r"),
    _ -> word
  }
}

fn digit_value(ch: source_char) -> i64 {
  match ch {
    D4 -> 4,
    D2 -> 2,
    _ -> 0
  }
}

fn flush(state: scan_state, tokens: List<lexeme>) -> List<lexeme> {
  match state {
    Start -> tokens,
    InIdent(word) -> [Word(word), ..tokens],
    InNumber(value) -> [Number(value), ..tokens]
  }
}

fn scan(chars: List<source_char>, state: scan_state, tokens: List<lexeme>) -> List<lexeme> {
  match chars {
    [] -> flush(state, tokens),
    [ch, ..rest] ->
      match ch {
        Space -> scan(rest, Start, flush(state, tokens)),
        LParen -> scan(rest, Start, [Symbol("("), ..flush(state, tokens)]),
        RParen -> scan(rest, Start, [Symbol(")"), ..flush(state, tokens)]),
        LBrace -> scan(rest, Start, [Symbol("{"), ..flush(state, tokens)]),
        RBrace -> scan(rest, Start, [Symbol("}"), ..flush(state, tokens)]),
        Equal -> scan(rest, Start, [Symbol("="), ..flush(state, tokens)]),
        _ ->
          if is_letter(ch) {
            match state {
              Start -> scan(rest, InIdent(append_letter("", ch)), tokens),
              InIdent(word) -> scan(rest, InIdent(append_letter(word, ch)), tokens),
              InNumber(_) -> scan(rest, InIdent(append_letter("", ch)), flush(state, tokens))
            }
          } else {
            if is_digit(ch) {
              match state {
                Start -> scan(rest, InNumber(digit_value(ch)), tokens),
                InIdent(_) -> scan(rest, InNumber(digit_value(ch)), flush(state, tokens)),
                InNumber(value) -> scan(rest, InNumber((value * 10) + digit_value(ch)), tokens)
              }
            } else {
              scan(rest, Start, tokens)
            }
          }
      }
  }
}

fn add(total: summary, token: lexeme) -> summary {
  match token {
    Word(word) ->
      if word == "fn" {
        summary { functions: total.functions + 1, bindings: total.bindings, identifiers: total.identifiers, literals: total.literals, symbols: total.symbols }
      } else {
        if word == "let" {
          summary { functions: total.functions, bindings: total.bindings + 1, identifiers: total.identifiers, literals: total.literals, symbols: total.symbols }
        } else {
          summary { functions: total.functions, bindings: total.bindings, identifiers: total.identifiers + 1, literals: total.literals, symbols: total.symbols }
        }
      },
    Number(_) -> summary { functions: total.functions, bindings: total.bindings, identifiers: total.identifiers, literals: total.literals + 1, symbols: total.symbols },
    Symbol(_) -> summary { functions: total.functions, bindings: total.bindings, identifiers: total.identifiers, literals: total.literals, symbols: total.symbols + 1 }
  }
}

fn summarize(tokens: List<lexeme>, total: summary) -> summary {
  match tokens {
    [] -> total,
    [token, ..rest] -> summarize(rest, add(total, token))
  }
}

fn main() {
  let source = [F, N, Space, M, A, I, N, LParen, RParen, Space, LBrace, Space, L, E, T, Space, A, N, S, W, E, R, Space, Equal, Space, D4, D2, Space, RBrace];
  let tokens = scan(source, Start, []);
  let total = summarize(tokens, summary { functions: 0, bindings: 0, identifiers: 0, literals: 0, symbols: 0 });
  dbg(total.functions);
  dbg(total.bindings);
  dbg(total.identifiers);
  dbg(total.literals);
  dbg(total.symbols)
}
