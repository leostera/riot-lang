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
  | Comma

type expr =
  | Var(String)
  | Int(i64)
  | Call(String, List<expr>)

type stmt =
  | LetStmt(String, expr)
  | ExprStmt(expr)

type decl = Function(String, List<stmt>)

type expr_parse =
  | ExprOk(expr, List<token>)
  | ExprErr(String)

type args_parse =
  | ArgsOk(List<expr>, List<token>)
  | ArgsErr(String)

type stmt_parse =
  | StmtOk(stmt, List<token>)
  | StmtErr(String)

type stmts_parse =
  | StmtsOk(List<stmt>, List<token>)
  | StmtsErr(String)

type decl_parse =
  | DeclOk(decl, List<token>)
  | DeclErr(String)

type summary = { functions: i64, lets: i64, calls: i64, literals: i64, errors: i64 }

fn parse_args(tokens: List<token>, args: List<expr>) -> args_parse {
  match tokens {
    [] -> ArgsErr("missing )"),
    [RParen, ..rest] -> ArgsOk(args, rest),
    _ ->
      match parse_expr(tokens) {
        ExprErr(message) -> ArgsErr(message),
        ExprOk(arg, rest) ->
          match rest {
            [Comma, ..more] -> parse_args(more, [arg, ..args]),
            [RParen, ..more] -> ArgsOk([arg, ..args], more),
            _ -> ArgsErr("missing comma or )")
          }
      }
  }
}

fn parse_ident_tail(name: String, tokens: List<token>) -> expr_parse {
  match tokens {
    [LParen, ..rest] ->
      match parse_args(rest, []) {
        ArgsErr(message) -> ExprErr(message),
        ArgsOk(args, more) -> ExprOk(Call(name, args), more)
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

fn parse_stmts(tokens: List<token>, stmts: List<stmt>) -> stmts_parse {
  match tokens {
    [] -> StmtsErr("missing }"),
    [RBrace, ..rest] -> StmtsOk(stmts, rest),
    _ ->
      match parse_stmt(tokens) {
        StmtErr(message) -> StmtsErr(message),
        StmtOk(stmt, rest) -> parse_stmts(rest, [stmt, ..stmts])
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

fn count_expr(value: expr, total: summary) -> summary {
  match value {
    Var(_) -> total,
    Int(_) -> summary { functions: total.functions, lets: total.lets, calls: total.calls, literals: total.literals + 1, errors: total.errors },
    Call(_, args) -> count_exprs(args, summary { functions: total.functions, lets: total.lets, calls: total.calls + 1, literals: total.literals, errors: total.errors })
  }
}

fn count_exprs(args: List<expr>, total: summary) -> summary {
  match args {
    [] -> total,
    [arg, ..rest] -> count_exprs(rest, count_expr(arg, total))
  }
}

fn count_stmts(stmts: List<stmt>, total: summary) -> summary {
  match stmts {
    [] -> total,
    [stmt, ..rest] ->
      match stmt {
        LetStmt(_, value) -> count_stmts(rest, count_expr(value, summary { functions: total.functions, lets: total.lets + 1, calls: total.calls, literals: total.literals, errors: total.errors })),
        ExprStmt(value) -> count_stmts(rest, count_expr(value, total))
      }
  }
}

fn summarize(parsed: decl_parse) -> summary {
  match parsed {
    DeclErr(_) -> summary { functions: 0, lets: 0, calls: 0, literals: 0, errors: 1 },
    DeclOk(decl, rest) ->
      match decl {
        Function(_, stmts) ->
          match rest {
            [] -> count_stmts(stmts, summary { functions: 1, lets: 0, calls: 0, literals: 0, errors: 0 }),
            _ -> summary { functions: 1, lets: 0, calls: 0, literals: 0, errors: 1 }
          }
      }
  }
}

fn main() {
  let tokens = [KwFn, Ident("main"), LParen, RParen, LBrace, KwLet, Ident("answer"), Equal, Number(42), Ident("print"), LParen, Ident("answer"), RParen, RBrace];
  let total = summarize(parse_function(tokens));
  dbg(total.functions);
  dbg(total.lets);
  dbg(total.calls);
  dbg(total.literals);
  dbg(total.errors)
}
