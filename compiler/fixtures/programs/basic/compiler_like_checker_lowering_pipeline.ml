type typ =
  | TInt
  | TString
  | TUnit
  | TUnknown
  | TFn(typ, typ)

type expr =
  | IntLit(i64)
  | StringLit(String)
  | Var(String)
  | Call(String, expr)

type stmt =
  | LetStmt(String, expr)
  | ExprStmt(expr)

type binding = Binding(String, typ)

type typed_expr = TypedExpr(expr, typ)

type slot =
  | ScalarSlot(String)
  | ValueSlot(String)

type op =
  | ConstI64
  | ConstString
  | LoadLocal
  | CallOp
  | UnknownOp

type summary = { i64s: i64, strings: i64, unknowns: i64, calls: i64, scalar_slots: i64, value_slots: i64 }

type state = { env: List<binding>, summary: summary }

type lookup_result =
  | Found(typ)
  | Missing

fn same_type(left: typ, right: typ) -> bool {
  match left {
    TInt -> match right { TInt -> true, _ -> false },
    TString -> match right { TString -> true, _ -> false },
    TUnit -> match right { TUnit -> true, _ -> false },
    TUnknown -> match right { TUnknown -> true, _ -> false },
    TFn(left_arg, left_ret) ->
      match right {
        TFn(right_arg, right_ret) -> same_type(left_arg, right_arg) && same_type(left_ret, right_ret),
        _ -> false
      }
  }
}

fn lookup(name: String, env: List<binding>) -> lookup_result {
  match env {
    [] -> Missing,
    [Binding(bound, ty), ..rest] ->
      if bound == name {
        Found(ty)
      } else {
        lookup(name, rest)
      }
  }
}

fn bump_i64(total: summary) -> summary {
  summary { i64s: total.i64s + 1, strings: total.strings, unknowns: total.unknowns, calls: total.calls, scalar_slots: total.scalar_slots, value_slots: total.value_slots }
}

fn bump_string(total: summary) -> summary {
  summary { i64s: total.i64s, strings: total.strings + 1, unknowns: total.unknowns, calls: total.calls, scalar_slots: total.scalar_slots, value_slots: total.value_slots }
}

fn bump_unknown(total: summary) -> summary {
  summary { i64s: total.i64s, strings: total.strings, unknowns: total.unknowns + 1, calls: total.calls, scalar_slots: total.scalar_slots, value_slots: total.value_slots }
}

fn bump_call(total: summary) -> summary {
  summary { i64s: total.i64s, strings: total.strings, unknowns: total.unknowns, calls: total.calls + 1, scalar_slots: total.scalar_slots, value_slots: total.value_slots }
}

fn bump_scalar_slot(total: summary) -> summary {
  summary { i64s: total.i64s, strings: total.strings, unknowns: total.unknowns, calls: total.calls, scalar_slots: total.scalar_slots + 1, value_slots: total.value_slots }
}

fn bump_value_slot(total: summary) -> summary {
  summary { i64s: total.i64s, strings: total.strings, unknowns: total.unknowns, calls: total.calls, scalar_slots: total.scalar_slots, value_slots: total.value_slots + 1 }
}

fn classify_value(ty: typ, total: summary) -> summary {
  match ty {
    TInt -> bump_i64(total),
    TString -> bump_string(total),
    TUnit -> total,
    TFn(_, _) -> bump_string(total),
    TUnknown -> bump_unknown(total)
  }
}

fn classify_slot(ty: typ, total: summary) -> summary {
  match ty {
    TInt -> bump_scalar_slot(total),
    _ -> bump_value_slot(total)
  }
}

fn check_expr(value: expr, env: List<binding>) -> typed_expr {
  match value {
    IntLit(_) -> TypedExpr(value, TInt),
    StringLit(_) -> TypedExpr(value, TString),
    Var(name) ->
      match lookup(name, env) {
        Missing -> TypedExpr(value, TUnknown),
        Found(ty) -> TypedExpr(value, ty)
      },
    Call(name, arg) ->
      match lookup(name, env) {
        Found(TFn(expected, result)) ->
          match check_expr(arg, env) {
            TypedExpr(_, actual) ->
              if same_type(expected, actual) {
                TypedExpr(value, result)
              } else {
                TypedExpr(value, TUnknown)
              }
          },
        _ -> TypedExpr(value, TUnknown)
      }
  }
}

fn lower_expr(typed: typed_expr, state: state) -> state {
  match typed {
    TypedExpr(expr, ty) ->
      match expr {
        Call(_, arg) -> lower_expr(check_expr(arg, state.env), state { env: state.env, summary: bump_call(classify_value(ty, state.summary)) }),
        _ -> state { env: state.env, summary: classify_value(ty, state.summary) }
      }
  }
}

fn bind_local(name: String, ty: typ, state: state) -> state {
  state { env: [Binding(name, ty), ..state.env], summary: classify_slot(ty, state.summary) }
}

fn check_stmt(stmt: stmt, state: state) -> state {
  match stmt {
    LetStmt(name, value) ->
      match check_expr(value, state.env) {
        TypedExpr(_, ty) -> bind_local(name, ty, lower_expr(TypedExpr(value, ty), state))
      },
    ExprStmt(value) -> lower_expr(check_expr(value, state.env), state)
  }
}

fn check_stmts(stmts: List<stmt>, state: state) -> state {
  match stmts {
    [] -> state,
    [stmt, ..rest] -> check_stmts(rest, check_stmt(stmt, state))
  }
}

fn main() {
  let initial = state { env: [Binding("len", TFn(TString, TInt)), Binding("show", TFn(TInt, TString))], summary: summary { i64s: 0, strings: 0, unknowns: 0, calls: 0, scalar_slots: 0, value_slots: 0 } };
  let program = [LetStmt("name", StringLit("riot")), LetStmt("size", Call("len", Var("name"))), LetStmt("rendered", Call("show", Var("size"))), LetStmt("bad", Call("len", Var("size"))), ExprStmt(Var("missing"))];
  let checked = check_stmts(program, initial);
  dbg(checked.summary.i64s);
  dbg(checked.summary.strings);
  dbg(checked.summary.unknowns);
  dbg(checked.summary.calls);
  dbg(checked.summary.scalar_slots);
  dbg(checked.summary.value_slots)
}
