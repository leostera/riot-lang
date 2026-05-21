type token =
  | KwFn
  | KwLet
  | Ident(String)
  | IntLit(i64)
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Equal
  | Eof

type summary = { functions: i64, bindings: i64, identifiers: i64, literals: i64 }

fn classify(word: String) -> token {
  if word == "fn" {
    KwFn
  } else {
    if word == "let" {
      KwLet
    } else {
      if word == "(" {
        LParen
      } else {
        if word == ")" {
          RParen
        } else {
          if word == "{" {
            LBrace
          } else {
            if word == "}" {
              RBrace
            } else {
              if word == "=" {
                Equal
              } else {
                if word == "123" {
                  IntLit(123)
                } else {
                  Ident(word)
                }
              }
            }
          }
        }
      }
    }
  }
}

fn add_token(total: summary, token: token) -> summary {
  match token {
    KwFn -> summary { functions: total.functions + 1, bindings: total.bindings, identifiers: total.identifiers, literals: total.literals },
    KwLet -> summary { functions: total.functions, bindings: total.bindings + 1, identifiers: total.identifiers, literals: total.literals },
    Ident(_) -> summary { functions: total.functions, bindings: total.bindings, identifiers: total.identifiers + 1, literals: total.literals },
    IntLit(_) -> summary { functions: total.functions, bindings: total.bindings, identifiers: total.identifiers, literals: total.literals + 1 },
    _ -> total
  }
}

fn summarize(words: List<String>, total: summary) -> summary {
  match words {
    [] -> total,
    [word, ..rest] -> summarize(rest, add_token(total, classify(word)))
  }
}

fn main() {
  let words = ["fn", "main", "(", ")", "{", "let", "answer", "=", "123", "}"];
  let total = summarize(words, summary { functions: 0, bindings: 0, identifiers: 0, literals: 0 });
  dbg(total.functions);
  dbg(total.bindings);
  dbg(total.identifiers);
  dbg(total.literals)
}
