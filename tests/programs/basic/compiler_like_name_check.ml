type expr =
  | Name(String)
  | Let(String, expr, expr)
  | Pair(expr, expr)

type binding = { name: String }

type diagnostic = { message: String }

fn contains(name: String, scope: List<binding>) -> bool {
  match scope {
    [] -> false,
    [entry, ..rest] ->
      if entry.name == name {
        true
      } else {
        contains(name, rest)
      }
  }
}

fn append(left: List<diagnostic>, right: List<diagnostic>) -> List<diagnostic> {
  match left {
    [] -> right,
    [item, ..rest] -> [item, ..append(rest, right)]
  }
}

fn check_expr(expr: expr, scope: List<binding>) -> List<diagnostic> {
  match expr {
    Name(name) ->
      if contains(name, scope) {
        []
      } else {
        [diagnostic { message: string_concat("unknown name: ", name) }]
      },
    Pair(left, right) -> append(check_expr(left, scope), check_expr(right, scope)),
    Let(name, value, body) -> append(check_expr(value, scope), check_expr(body, [binding { name: name }, ..scope]))
  }
}

fn render_diagnostics(diagnostics: List<diagnostic>) -> String {
  match diagnostics {
    [] -> "ok",
    [diagnostic, ..rest] ->
      match rest {
        [] -> diagnostic.message,
        _ -> string_concat(diagnostic.message, string_concat("; ", render_diagnostics(rest)))
      }
  }
}

fn main() {
  let scope = [binding { name: "input" }];
  let expr = Let("tmp", Name("input"), Pair(Name("tmp"), Name("missing")));
  println(render_diagnostics(check_expr(expr, scope)))
}
