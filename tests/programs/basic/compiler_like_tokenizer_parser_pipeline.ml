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
  | P
  | D4
  | D2
  | Space
  | LParenChar
  | RParenChar
  | LBraceChar
  | RBraceChar
  | EqualChar

type token =
  | KwFn
  | KwLet
  | Ident(String)
  | Number(i64)
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Equal

type scan_state =
  | Start
  | InWord(String)
  | InNumber(i64)

type expr =
  | Var(String)
  | Int(i64)
  | Call(String, expr)

type stmt =
  | LetStmt(String, expr)
  | ExprStmt(expr)

type decl = Function(String, List<stmt>)

type expr_parse =
  | ExprOk(expr, List<token>)
  | ExprErr(String)

type stmt_parse =
  | StmtOk(stmt, List<token>)
  | StmtErr(String)

type stmts_parse =
  | StmtsOk(List<stmt>, List<token>)
  | StmtsErr(String)

type decl_parse =
  | DeclOk(decl, List<token>)
  | DeclErr(String)

type summary = { tokens: i64, functions: i64, lets: i64, calls: i64, literals: i64, errors: i64 }

fn append_token(tokens: List<token>, token: token) -> List<token> {
  match tokens {
    [] -> [token],
    [head, ..tail] -> [head, ..append_token(tail, token)]
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
    P -> string_concat(word, "p"),
    _ -> word
  }
}

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
    P -> true,
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

fn digit_value(ch: source_char) -> i64 {
  match ch {
    D4 -> 4,
    D2 -> 2,
    _ -> 0
  }
}

fn finish_word(word: String) -> token {
  if word == "fn" {
    KwFn
  } else {
    if word == "let" {
      KwLet
    } else {
      Ident(word)
    }
  }
}

fn flush(state: scan_state, tokens: List<token>) -> List<token> {
  match state {
    Start -> tokens,
    InWord(word) -> append_token(tokens, finish_word(word)),
    InNumber(value) -> append_token(tokens, Number(value))
  }
}

fn scan_symbol(ch: source_char, tokens: List<token>) -> List<token> {
  match ch {
    LParenChar -> append_token(tokens, LParen),
    RParenChar -> append_token(tokens, RParen),
    LBraceChar -> append_token(tokens, LBrace),
    RBraceChar -> append_token(tokens, RBrace),
    EqualChar -> append_token(tokens, Equal),
    _ -> tokens
  }
}

fn scan(chars: List<source_char>, state: scan_state, tokens: List<token>) -> List<token> {
  match chars {
    [] -> flush(state, tokens),
    [ch, ..rest] ->
      if is_letter(ch) {
        match state {
          Start -> scan(rest, InWord(append_letter("", ch)), tokens),
          InWord(word) -> scan(rest, InWord(append_letter(word, ch)), tokens),
          InNumber(_) -> scan(rest, InWord(append_letter("", ch)), flush(state, tokens))
        }
      } else {
        if is_digit(ch) {
          match state {
            Start -> scan(rest, InNumber(digit_value(ch)), tokens),
            InWord(_) -> scan(rest, InNumber(digit_value(ch)), flush(state, tokens)),
            InNumber(value) -> scan(rest, InNumber((value * 10) + digit_value(ch)), tokens)
          }
        } else {
          match ch {
            Space -> scan(rest, Start, flush(state, tokens)),
            _ -> scan(rest, Start, scan_symbol(ch, flush(state, tokens)))
          }
        }
      }
  }
}

fn parse_ident_tail(name: String, tokens: List<token>) -> expr_parse {
  match tokens {
    [LParen, ..rest] ->
      match parse_expr(rest) {
        ExprErr(message) -> ExprErr(message),
        ExprOk(arg, more) ->
          match more {
            [RParen, ..tail] -> ExprOk(Call(name, arg), tail),
            _ -> ExprErr("missing )")
          }
      },
    _ -> ExprOk(Var(name), tokens)
  }
}

fn parse_expr(tokens: List<token>) -> expr_parse {
  match tokens {
    [] -> ExprErr("missing expression"),
    [Ident(name), ..rest] -> parse_ident_tail(name, rest),
    [Number(value), ..rest] -> ExprOk(Int(value), rest),
    _ -> ExprErr("unexpected token")
  }
}

fn parse_stmt(tokens: List<token>) -> stmt_parse {
  match tokens {
    [KwLet, Ident(name), Equal, ..rest] ->
      match parse_expr(rest) {
        ExprErr(message) -> StmtErr(message),
        ExprOk(value, more) -> StmtOk(LetStmt(name, value), more)
      },
    _ ->
      match parse_expr(tokens) {
        ExprErr(message) -> StmtErr(message),
        ExprOk(value, more) -> StmtOk(ExprStmt(value), more)
      }
  }
}

fn append_stmt(stmts: List<stmt>, stmt: stmt) -> List<stmt> {
  match stmts {
    [] -> [stmt],
    [head, ..tail] -> [head, ..append_stmt(tail, stmt)]
  }
}

fn parse_stmts(tokens: List<token>, stmts: List<stmt>) -> stmts_parse {
  match tokens {
    [] -> StmtsErr("missing }"),
    [RBrace, ..rest] -> StmtsOk(stmts, rest),
    _ ->
      match parse_stmt(tokens) {
        StmtErr(message) -> StmtsErr(message),
        StmtOk(stmt, rest) -> parse_stmts(rest, append_stmt(stmts, stmt))
      }
  }
}

fn parse_function(tokens: List<token>) -> decl_parse {
  match tokens {
    [KwFn, Ident(name), LParen, RParen, LBrace, ..body] ->
      match parse_stmts(body, []) {
        StmtsErr(message) -> DeclErr(message),
        StmtsOk(stmts, rest) -> DeclOk(Function(name, stmts), rest)
      },
    _ -> DeclErr("expected function declaration")
  }
}

fn count_tokens(tokens: List<token>) -> i64 {
  match tokens {
    [] -> 0,
    [_, ..rest] -> 1 + count_tokens(rest)
  }
}

fn count_expr(value: expr, total: summary) -> summary {
  match value {
    Var(_) -> total,
    Int(_) -> summary { tokens: total.tokens, functions: total.functions, lets: total.lets, calls: total.calls, literals: total.literals + 1, errors: total.errors },
    Call(_, arg) -> count_expr(arg, summary { tokens: total.tokens, functions: total.functions, lets: total.lets, calls: total.calls + 1, literals: total.literals, errors: total.errors })
  }
}

fn count_stmts(stmts: List<stmt>, total: summary) -> summary {
  match stmts {
    [] -> total,
    [stmt, ..rest] ->
      match stmt {
        LetStmt(_, value) -> count_stmts(rest, count_expr(value, summary { tokens: total.tokens, functions: total.functions, lets: total.lets + 1, calls: total.calls, literals: total.literals, errors: total.errors })),
        ExprStmt(value) -> count_stmts(rest, count_expr(value, total))
      }
  }
}

fn summarize(tokens: List<token>, parsed: decl_parse) -> summary {
  match parsed {
    DeclErr(_) -> summary { tokens: count_tokens(tokens), functions: 0, lets: 0, calls: 0, literals: 0, errors: 1 },
    DeclOk(decl, rest) ->
      match decl {
        Function(_, stmts) ->
          match rest {
            [] -> count_stmts(stmts, summary { tokens: count_tokens(tokens), functions: 1, lets: 0, calls: 0, literals: 0, errors: 0 }),
            _ -> summary { tokens: count_tokens(tokens), functions: 1, lets: 0, calls: 0, literals: 0, errors: 1 }
          }
      }
  }
}

fn pipeline(chars: List<source_char>) -> summary {
  let tokens = scan(chars, Start, []);
  summarize(tokens, parse_function(tokens))
}

fn main() {
  let source = [F, N, Space, M, A, I, N, LParenChar, RParenChar, Space, LBraceChar, Space, L, E, T, Space, A, N, S, W, E, R, Space, EqualChar, Space, D4, D2, Space, P, R, I, N, T, LParenChar, A, N, S, W, E, R, RParenChar, Space, RBraceChar];
  let total = pipeline(source);
  let broken = pipeline([F, N, Space, M, A, I, N, LParenChar, RParenChar, Space, LBraceChar, Space, L, E, T, Space, A, N, S, W, E, R, Space, EqualChar, Space, D4, D2]);
  dbg(total.tokens);
  dbg(total.functions);
  dbg(total.lets);
  dbg(total.calls);
  dbg(total.literals);
  dbg(total.errors);
  dbg(broken.errors)
}
