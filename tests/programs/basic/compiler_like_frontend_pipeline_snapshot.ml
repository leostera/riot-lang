type token =
  | Fn
  | Let
  | Ident(String)
  | Int(i64)
  | LBrace
  | RBrace
  | Equal
  | Unknown(String)

type parse_state = {
  tokens: i64,
  functions: i64,
  lets: i64,
  literals: i64,
  errors: i64,
  open_braces: i64
}

type frontend_snapshot = {
  modules: i64,
  tokens: i64,
  functions: i64,
  lets: i64,
  literals: i64,
  parse_errors: i64,
  typed_nodes: i64,
  value_slots: i64
}

fn empty_parse_state() -> parse_state {
  parse_state { tokens: 0, functions: 0, lets: 0, literals: 0, errors: 0, open_braces: 0 }
}

fn empty_snapshot() -> frontend_snapshot {
  frontend_snapshot { modules: 0, tokens: 0, functions: 0, lets: 0, literals: 0, parse_errors: 0, typed_nodes: 0, value_slots: 0 }
}

fn bump_error(state: parse_state) -> parse_state {
  parse_state {
    tokens: state.tokens,
    functions: state.functions,
    lets: state.lets,
    literals: state.literals,
    errors: state.errors + 1,
    open_braces: state.open_braces
  }
}

fn consume_token(state: parse_state, token: token) -> parse_state {
  match token {
    Fn -> parse_state { tokens: state.tokens + 1, functions: state.functions + 1, lets: state.lets, literals: state.literals, errors: state.errors, open_braces: state.open_braces },
    Let -> parse_state { tokens: state.tokens + 1, functions: state.functions, lets: state.lets + 1, literals: state.literals, errors: state.errors, open_braces: state.open_braces },
    Int(_) -> parse_state { tokens: state.tokens + 1, functions: state.functions, lets: state.lets, literals: state.literals + 1, errors: state.errors, open_braces: state.open_braces },
    LBrace -> parse_state { tokens: state.tokens + 1, functions: state.functions, lets: state.lets, literals: state.literals, errors: state.errors, open_braces: state.open_braces + 1 },
    RBrace ->
      if state.open_braces == 0 {
        bump_error(parse_state { tokens: state.tokens + 1, functions: state.functions, lets: state.lets, literals: state.literals, errors: state.errors, open_braces: state.open_braces })
      } else {
        parse_state { tokens: state.tokens + 1, functions: state.functions, lets: state.lets, literals: state.literals, errors: state.errors, open_braces: state.open_braces - 1 }
      },
    Unknown(_) -> bump_error(parse_state { tokens: state.tokens + 1, functions: state.functions, lets: state.lets, literals: state.literals, errors: state.errors, open_braces: state.open_braces }),
    _ -> parse_state { tokens: state.tokens + 1, functions: state.functions, lets: state.lets, literals: state.literals, errors: state.errors, open_braces: state.open_braces }
  }
}

fn parse_tokens(tokens: List<token>, state: parse_state) -> parse_state {
  match tokens {
    [] -> parse_state {
      tokens: state.tokens,
      functions: state.functions,
      lets: state.lets,
      literals: state.literals,
      errors: state.errors + state.open_braces,
      open_braces: 0
    },
    [token, ..rest] -> parse_tokens(rest, consume_token(state, token))
  }
}

fn add_module(total: frontend_snapshot, parsed: parse_state) -> frontend_snapshot {
  frontend_snapshot {
    modules: total.modules + 1,
    tokens: total.tokens + parsed.tokens,
    functions: total.functions + parsed.functions,
    lets: total.lets + parsed.lets,
    literals: total.literals + parsed.literals,
    parse_errors: total.parse_errors + parsed.errors,
    typed_nodes: total.typed_nodes + parsed.functions + parsed.lets + parsed.literals,
    value_slots: total.value_slots + parsed.lets + parsed.literals
  }
}

fn summarize_modules(files: List<List<token>>, total: frontend_snapshot) -> frontend_snapshot {
  match files {
    [] -> total,
    [tokens, ..rest] -> summarize_modules(rest, add_module(total, parse_tokens(tokens, empty_parse_state())))
  }
}

fn main() {
  let syntax = [Fn, Ident("classify"), LBrace, Let, Ident("tag"), Equal, Int(42), RBrace];
  let parser = [Fn, Ident("parse"), LBrace, Let, Ident("node"), Equal, Int(1), Unknown("missing-call-close")];
  let snapshot = summarize_modules([syntax, parser], empty_snapshot());
  dbg(snapshot.modules);
  dbg(snapshot.tokens);
  dbg(snapshot.functions);
  dbg(snapshot.lets);
  dbg(snapshot.literals);
  dbg(snapshot.parse_errors);
  dbg(snapshot.typed_nodes);
  dbg(snapshot.value_slots)
}
