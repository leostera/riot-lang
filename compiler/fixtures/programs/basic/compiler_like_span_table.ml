type token =
  | Ident(String)
  | Keyword(String)
  | Symbol(String)

type span = { text: String, line: i64, column: i64 }

fn token_text(token: token) -> String {
  match token {
    Ident(text) -> text,
    Keyword(text) -> text,
    Symbol(text) -> text
  }
}

fn annotate_tokens(tokens: List<token>, line: i64, column: i64) -> List<span> {
  match tokens {
    [] -> [],
    [token, ..rest] -> [span { text: token_text(token), line: line, column: column }, ..annotate_tokens(rest, line, column + 1)]
  }
}

fn render_spans(spans: List<span>) -> String {
  match spans {
    [] -> "",
    [entry, ..rest] ->
      match rest {
        [] -> entry.text,
        _ -> string_concat(entry.text, string_concat(",", render_spans(rest)))
      }
  }
}

fn main() {
  let tokens = [Keyword("let"), Ident("answer"), Symbol("=")];
  println(render_spans(annotate_tokens(tokens, 4, 1)))
}
