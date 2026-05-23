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

type fn_sig = FnSig(String, i64)

type diag =
  | UnknownValue(String)
  | ArityMismatch(String, i64, i64)

type check_summary = { functions: i64, lets: i64, calls: i64, literals: i64, unknowns: i64, arities: i64, parse_errors: i64 }

type check_state = { vars: List<String>, fns: List<fn_sig>, summary: check_summary }

type arity_lookup =
  | FoundArity(i64)
  | MissingArity

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

fn append_stmt(stmts: List<stmt>, next: stmt) -> List<stmt> {
  match stmts {
    [] -> [next],
    [stmt, ..rest] -> [stmt, ..append_stmt(rest, next)]
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

fn count_args(args: List<expr>) -> i64 {
  match args {
    [] -> 0,
    [_, ..rest] -> 1 + count_args(rest)
  }
}

fn contains_name(names: List<String>, target: String) -> bool {
  match names {
    [] -> false,
    [name, ..rest] ->
      if name == target {
        true
      } else {
        contains_name(rest, target)
      }
  }
}

fn lookup_arity(fns: List<fn_sig>, target: String) -> arity_lookup {
  match fns {
    [] -> MissingArity,
    [FnSig(name, arity), ..rest] ->
      if name == target {
        FoundArity(arity)
      } else {
        lookup_arity(rest, target)
      }
  }
}

fn bump_unknown(total: check_summary) -> check_summary {
  check_summary { functions: total.functions, lets: total.lets, calls: total.calls, literals: total.literals, unknowns: total.unknowns + 1, arities: total.arities, parse_errors: total.parse_errors }
}

fn bump_arity(total: check_summary) -> check_summary {
  check_summary { functions: total.functions, lets: total.lets, calls: total.calls, literals: total.literals, unknowns: total.unknowns, arities: total.arities + 1, parse_errors: total.parse_errors }
}

fn bump_literal(total: check_summary) -> check_summary {
  check_summary { functions: total.functions, lets: total.lets, calls: total.calls, literals: total.literals + 1, unknowns: total.unknowns, arities: total.arities, parse_errors: total.parse_errors }
}

fn bump_call(total: check_summary) -> check_summary {
  check_summary { functions: total.functions, lets: total.lets, calls: total.calls + 1, literals: total.literals, unknowns: total.unknowns, arities: total.arities, parse_errors: total.parse_errors }
}

fn check_expr(value: expr, state: check_state) -> check_state {
  match value {
    Int(_) -> check_state { vars: state.vars, fns: state.fns, summary: bump_literal(state.summary) },
    Var(name) ->
      if contains_name(state.vars, name) {
        state
      } else {
        check_state { vars: state.vars, fns: state.fns, summary: bump_unknown(state.summary) }
      },
    Call(name, args) ->
      check_call_args(args, check_call_shape(name, count_args(args), check_state { vars: state.vars, fns: state.fns, summary: bump_call(state.summary) }))
  }
}

fn check_call_shape(name: String, actual: i64, state: check_state) -> check_state {
  match lookup_arity(state.fns, name) {
    MissingArity -> check_state { vars: state.vars, fns: state.fns, summary: bump_unknown(state.summary) },
    FoundArity(expected) ->
      if expected == actual {
        state
      } else {
        check_state { vars: state.vars, fns: state.fns, summary: bump_arity(state.summary) }
      }
  }
}

fn check_call_args(args: List<expr>, state: check_state) -> check_state {
  match args {
    [] -> state,
    [arg, ..rest] -> check_call_args(rest, check_expr(arg, state))
  }
}

fn add_let(name: String, checked: check_state) -> check_state {
  check_state { vars: [name, ..checked.vars], fns: checked.fns, summary: check_summary { functions: checked.summary.functions, lets: checked.summary.lets + 1, calls: checked.summary.calls, literals: checked.summary.literals, unknowns: checked.summary.unknowns, arities: checked.summary.arities, parse_errors: checked.summary.parse_errors } }
}

fn check_let_stmt(name: String, value: expr, state: check_state) -> check_state {
  add_let(name, check_expr(value, state))
}

fn check_stmts(stmts: List<stmt>, state: check_state) -> check_state {
  match stmts {
    [] -> state,
    [stmt, ..rest] ->
      match stmt {
        LetStmt(name, value) -> check_stmts(rest, check_let_stmt(name, value, state)),
        ExprStmt(value) -> check_stmts(rest, check_expr(value, state))
      }
  }
}

fn check_decl(parsed: decl_parse, state: check_state) -> check_state {
  match parsed {
    DeclErr(_) -> check_state { vars: state.vars, fns: state.fns, summary: check_summary { functions: state.summary.functions, lets: state.summary.lets, calls: state.summary.calls, literals: state.summary.literals, unknowns: state.summary.unknowns, arities: state.summary.arities, parse_errors: state.summary.parse_errors + 1 } },
    DeclOk(decl, rest) ->
      match decl {
        Function(_, stmts) ->
          match rest {
            [] -> check_stmts(stmts, check_state { vars: [], fns: state.fns, summary: check_summary { functions: state.summary.functions + 1, lets: state.summary.lets, calls: state.summary.calls, literals: state.summary.literals, unknowns: state.summary.unknowns, arities: state.summary.arities, parse_errors: state.summary.parse_errors } }),
            _ -> check_state { vars: state.vars, fns: state.fns, summary: check_summary { functions: state.summary.functions + 1, lets: state.summary.lets, calls: state.summary.calls, literals: state.summary.literals, unknowns: state.summary.unknowns, arities: state.summary.arities, parse_errors: state.summary.parse_errors + 1 } }
          }
      }
  }
}

fn main() {
  let ok = [KwFn, Ident("main"), LParen, RParen, LBrace, KwLet, Ident("answer"), Equal, Number(42), Ident("print"), LParen, Ident("answer"), RParen, Ident("missing"), LParen, Ident("answer"), RParen, Ident("print"), LParen, Ident("answer"), Comma, Number(1), RParen, RBrace];
  let broken = [KwLet, Ident("answer"), Equal, Number(42)];
  let initial = check_state { vars: [], fns: [FnSig("print", 1), FnSig("main", 0)], summary: check_summary { functions: 0, lets: 0, calls: 0, literals: 0, unknowns: 0, arities: 0, parse_errors: 0 } };
  let checked = check_decl(parse_function(broken), check_decl(parse_function(ok), initial));
  dbg(checked.summary.functions);
  dbg(checked.summary.lets);
  dbg(checked.summary.calls);
  dbg(checked.summary.literals);
  dbg(checked.summary.unknowns);
  dbg(checked.summary.arities);
  dbg(checked.summary.parse_errors)
}
