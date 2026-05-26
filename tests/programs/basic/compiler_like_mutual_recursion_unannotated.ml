type token = Ident(String) | Number(i64) | LParen | RParen

fn parse_expr(tokens) {
  match tokens {
    [] -> "empty",
    [Ident(name), .._] -> string_concat("ident:", name),
    [Number(_), .._] -> "number",
    [LParen, ..rest] -> parse_group(rest),
    [RParen, .._] -> "close"
  }
}

fn parse_group(tokens) {
  match tokens {
    [] -> "missing-close",
    [RParen, .._] -> "unit",
    [token, ..rest] -> string_concat(parse_expr([token]), string_concat(";", parse_group(rest)))
  }
}

fn main() {
  dbg(parse_expr([LParen, Ident("answer"), Number(42), RParen]))
}
