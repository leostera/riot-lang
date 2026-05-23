type stmt = Local(String, bool) | NestedActor(List<stmt>) | Receive | Expr(String)
type diag = UnsupportedAnnotation(String) | MissingReceive

type validation = { has_receive: bool, diagnostics: List<diag> }

fn append_diags(left: List<diag>, right: List<diag>) -> List<diag> {
  match left {
    [] -> right,
    [head, ..tail] -> [head, ..append_diags(tail, right)],
    _ -> right
  }
}

fn merge_validation(left: validation, right: validation) -> validation {
  validation { has_receive: left.has_receive || right.has_receive, diagnostics: append_diags(left.diagnostics, right.diagnostics) }
}

fn validate_actor_stmt(stmt: stmt) -> validation {
  match stmt {
    Local(name, annotated) -> validation { has_receive: false, diagnostics: if annotated { [UnsupportedAnnotation(name)] } else { [] } },
    NestedActor(body) -> validate_actor_block(body),
    Receive -> validation { has_receive: true, diagnostics: [] },
    Expr(_) -> validation { has_receive: false, diagnostics: [] }
  }
}

fn validate_actor_block(stmts: List<stmt>) -> validation {
  let checked = validate_actor_stmts(stmts);
  if checked.has_receive {
    checked
  } else {
    validation { has_receive: false, diagnostics: append_diags(checked.diagnostics, [MissingReceive]) }
  }
}

fn validate_actor_stmts(stmts: List<stmt>) -> validation {
  match stmts {
    [] -> validation { has_receive: false, diagnostics: [] },
    [head, ..tail] -> merge_validation(validate_actor_stmt(head), validate_actor_stmts(tail)),
    _ -> validation { has_receive: false, diagnostics: [] }
  }
}

fn render_diag(diag: diag) -> String {
  match diag {
    UnsupportedAnnotation(name) -> string_concat("unsupported-annotation:", name),
    MissingReceive -> "missing-receive"
  }
}

fn render_diags(diags: List<diag>) -> String {
  match diags {
    [] -> "",
    [diag] -> render_diag(diag),
    [diag, ..tail] -> string_concat(render_diag(diag), string_concat("\n", render_diags(tail))),
    _ -> ""
  }
}

fn main() {
  let direct = validate_actor_block([Local("count", true), Receive]);
  println(render_diags(direct.diagnostics));

  let nested = validate_actor_block([
    NestedActor([Local("inner_count", true), Receive]),
    Receive
  ]);
  println(render_diags(nested.diagnostics));

  let missing_receive = validate_actor_block([Local("state", false), Expr("dbg")]);
  println(render_diags(missing_receive.diagnostics))
}
