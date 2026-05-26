type typ =
  | TInt
  | TString
  | TUnit
  | TActor(String)

type op =
  | ConstI64(String)
  | ConstString(String)
  | Call(String, typ)
  | Spawn(String)
  | Send(String)
  | Return(typ)

type summary = {
  scalar_values: i64,
  boxed_values: i64,
  helper_symbols: i64,
  exported_symbols: i64,
  returns: i64
}

fn bump_scalar(total: summary) -> summary {
  summary { scalar_values: total.scalar_values + 1, boxed_values: total.boxed_values, helper_symbols: total.helper_symbols, exported_symbols: total.exported_symbols, returns: total.returns }
}

fn bump_boxed(total: summary) -> summary {
  summary { scalar_values: total.scalar_values, boxed_values: total.boxed_values + 1, helper_symbols: total.helper_symbols, exported_symbols: total.exported_symbols, returns: total.returns }
}

fn bump_helper(total: summary) -> summary {
  summary { scalar_values: total.scalar_values, boxed_values: total.boxed_values, helper_symbols: total.helper_symbols + 1, exported_symbols: total.exported_symbols, returns: total.returns }
}

fn bump_export(total: summary) -> summary {
  summary { scalar_values: total.scalar_values, boxed_values: total.boxed_values, helper_symbols: total.helper_symbols, exported_symbols: total.exported_symbols + 1, returns: total.returns }
}

fn bump_return(total: summary) -> summary {
  summary { scalar_values: total.scalar_values, boxed_values: total.boxed_values, helper_symbols: total.helper_symbols, exported_symbols: total.exported_symbols, returns: total.returns + 1 }
}

fn classify_type(ty: typ, total: summary) -> summary {
  match ty {
    TInt -> bump_scalar(total),
    TString -> bump_boxed(total),
    TUnit -> total,
    TActor(_) -> bump_boxed(total)
  }
}

fn emit_op(op: op, total: summary) -> summary {
  match op {
    ConstI64(_) -> bump_scalar(total),
    ConstString(_) -> bump_boxed(total),
    Call(_, result) -> bump_export(classify_type(result, total)),
    Spawn(_) -> bump_helper(bump_boxed(total)),
    Send(_) -> bump_helper(total),
    Return(ty) -> bump_return(classify_type(ty, total))
  }
}

fn emit_ops(ops: List<op>, total: summary) -> summary {
  match ops {
    [] -> total,
    [op, ..rest] -> emit_ops(rest, emit_op(op, total))
  }
}

fn main() {
  let empty = summary { scalar_values: 0, boxed_values: 0, helper_symbols: 0, exported_symbols: 0, returns: 0 };
  let ops = [ConstI64("line"), ConstString("token"), Call("classify", TString), Spawn("worker"), Send("token"), Return(TUnit)];
  let emitted = emit_ops(ops, empty);
  dbg(emitted.scalar_values);
  dbg(emitted.boxed_values);
  dbg(emitted.helper_symbols);
  dbg(emitted.exported_symbols);
  dbg(emitted.returns)
}
