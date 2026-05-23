type typ =
  | TBool
  | TI64
  | TUnit
  | TUnknown

type expr =
  | BoolLit(bool)
  | IntLit(i64)
  | Var(String)
  | LessThan(String, i64)

type stmt =
  | LetI64(String, i64)
  | Assign(String, expr)
  | Emit(expr)
  | IfStmt(expr, List<stmt>)
  | WhileStmt(expr, List<stmt>)

type binding = Binding(String, typ)

type summary = { condition_checks: i64, loop_blocks: i64, backedges: i64, safepoints: i64, diagnostics: i64, unknowns: i64, scalar_slots: i64, emits: i64 }

type state = { env: List<binding>, summary: summary }

type lookup_result =
  | Found(typ)
  | Missing

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

fn bind(name: String, ty: typ, env: List<binding>) -> List<binding> {
  [Binding(name, ty), ..env]
}

fn bump_condition(total: summary) -> summary {
  summary { condition_checks: total.condition_checks + 1, loop_blocks: total.loop_blocks, backedges: total.backedges, safepoints: total.safepoints, diagnostics: total.diagnostics, unknowns: total.unknowns, scalar_slots: total.scalar_slots, emits: total.emits }
}

fn bump_loop(total: summary) -> summary {
  summary { condition_checks: total.condition_checks, loop_blocks: total.loop_blocks + 1, backedges: total.backedges + 1, safepoints: total.safepoints + 1, diagnostics: total.diagnostics, unknowns: total.unknowns, scalar_slots: total.scalar_slots, emits: total.emits }
}

fn bump_diagnostic(total: summary) -> summary {
  summary { condition_checks: total.condition_checks, loop_blocks: total.loop_blocks, backedges: total.backedges, safepoints: total.safepoints, diagnostics: total.diagnostics + 1, unknowns: total.unknowns, scalar_slots: total.scalar_slots, emits: total.emits }
}

fn bump_unknown(total: summary) -> summary {
  summary { condition_checks: total.condition_checks, loop_blocks: total.loop_blocks, backedges: total.backedges, safepoints: total.safepoints, diagnostics: total.diagnostics, unknowns: total.unknowns + 1, scalar_slots: total.scalar_slots, emits: total.emits }
}

fn bump_slot(total: summary) -> summary {
  summary { condition_checks: total.condition_checks, loop_blocks: total.loop_blocks, backedges: total.backedges, safepoints: total.safepoints, diagnostics: total.diagnostics, unknowns: total.unknowns, scalar_slots: total.scalar_slots + 1, emits: total.emits }
}

fn bump_emit(total: summary) -> summary {
  summary { condition_checks: total.condition_checks, loop_blocks: total.loop_blocks, backedges: total.backedges, safepoints: total.safepoints, diagnostics: total.diagnostics, unknowns: total.unknowns, scalar_slots: total.scalar_slots, emits: total.emits + 1 }
}

fn type_expr(value: expr, env: List<binding>) -> typ {
  match value {
    BoolLit(_) -> TBool,
    IntLit(_) -> TI64,
    Var(name) ->
      match lookup(name, env) {
        Found(ty) -> ty,
        Missing -> TUnknown
      },
    LessThan(name, _) ->
      match lookup(name, env) {
        Found(TI64) -> TBool,
        _ -> TUnknown
      }
  }
}

fn check_condition(cond: expr, state: state) -> state {
  match type_expr(cond, state.env) {
    TBool -> state { env: state.env, summary: bump_condition(state.summary) },
    TUnknown -> state { env: state.env, summary: bump_diagnostic(bump_unknown(bump_condition(state.summary))) },
    _ -> state { env: state.env, summary: bump_diagnostic(bump_condition(state.summary)) }
  }
}

fn lower_stmt(stmt: stmt, state: state) -> state {
  match stmt {
    LetI64(name, _) -> state { env: bind(name, TI64, state.env), summary: bump_slot(state.summary) },
    Assign(_, _) -> state,
    Emit(_) -> state { env: state.env, summary: bump_emit(state.summary) },
    IfStmt(cond, body) ->
      match check_condition(cond, state) {
        checked -> lower_stmts(body, checked)
      },
    WhileStmt(cond, body) ->
      match check_condition(cond, state) {
        checked ->
          match type_expr(cond, state.env) {
            TBool -> lower_stmts(body, state { env: checked.env, summary: bump_loop(checked.summary) }),
            _ -> checked
          }
      }
  }
}

fn lower_stmts(stmts: List<stmt>, state: state) -> state {
  match stmts {
    [] -> state,
    [stmt, ..rest] -> lower_stmts(rest, lower_stmt(stmt, state))
  }
}

fn main() {
  let initial = state { env: [Binding("limit", TI64), Binding("flag", TBool)], summary: summary { condition_checks: 0, loop_blocks: 0, backedges: 0, safepoints: 0, diagnostics: 0, unknowns: 0, scalar_slots: 0, emits: 0 } };
  let program = [LetI64("i", 0), WhileStmt(LessThan("i", 3), [Emit(Var("i")), Assign("i", IntLit(1))]), WhileStmt(Var("i"), [Emit(Var("flag"))]), IfStmt(Var("flag"), [Emit(Var("limit"))]), WhileStmt(Var("missing"), [])];
  let lowered = lower_stmts(program, initial);
  dbg(lowered.summary.condition_checks);
  dbg(lowered.summary.loop_blocks);
  dbg(lowered.summary.backedges);
  dbg(lowered.summary.safepoints);
  dbg(lowered.summary.diagnostics);
  dbg(lowered.summary.unknowns);
  dbg(lowered.summary.scalar_slots);
  dbg(lowered.summary.emits)
}
