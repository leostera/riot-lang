type binding_kind =
  | Param
  | Local

type binding = { name: String, kind: binding_kind }

type expr =
  | Name(String)
  | Let(String, expr, expr)
  | Pair(expr, expr)

fn kind_label(kind: binding_kind) -> String {
  match kind {
    Param -> "param",
    Local -> "local"
  }
}

fn lookup(name: String, scope: List<binding>) -> String {
  match scope {
    [] -> "missing",
    [entry, ..rest] ->
      if entry.name == name {
        kind_label(entry.kind)
      } else {
        lookup(name, rest)
      }
  }
}

fn visit(expr: expr, scope: List<binding>) -> String {
  match expr {
    Name(name) -> lookup(name, scope),
    Pair(left, right) -> string_concat(visit(left, scope), string_concat(",", visit(right, scope))),
    Let(name, value, body) -> {
      let value_result = visit(value, scope);
      let body_result = visit(body, [binding { name: name, kind: Local }, ..scope]);
      string_concat(value_result, string_concat(";", body_result))
    }
  }
}

fn main() {
  let scope = [binding { name: "arg", kind: Param }];
  let expr = Let("tmp", Name("arg"), Pair(Name("tmp"), Name("missing")));
  println(visit(expr, scope))
}
