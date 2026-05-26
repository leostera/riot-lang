type token =
  | Whitespace
  | Ident(String)
  | Keyword(String)
  | Eof

fn meaningful(tokens: List<token>) -> List<token> {
  match tokens {
    [] -> [],
    [Whitespace, ..rest] -> meaningful(rest),
    [Eof, .._] -> [Eof],
    [token, ..rest] -> [token, ..meaningful(rest)]
  }
}

fn count(tokens: List<token>) -> i64 {
  match tokens {
    [] -> 0,
    [Eof, .._] -> 0,
    [_, ..rest] -> 1 + count(rest)
  }
}

fn render_first(tokens: List<token>) -> String {
  match tokens {
    [Keyword(name), .._] -> string_concat("kw:", name),
    [Ident(name), .._] -> string_concat("id:", name),
    [Eof, .._] -> "eof",
    _ -> "none"
  }
}

fn main() {
  let tokens = [Whitespace, Keyword("fn"), Whitespace, Ident("main"), Eof];
  let filtered = meaningful(tokens);
  println(render_first(filtered));
  dbg(count(filtered))
}
