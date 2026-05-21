type expr =
  | Name(String)
  | Call(String, List<expr>)
  | Let(String, expr, expr)

type rename = { from: String, to: String }

fn rename_name(table: List<rename>, name: String) -> String {
  match table {
    [] -> name,
    [entry, ..rest] ->
      if entry.from == name {
        entry.to
      } else {
        rename_name(rest, name)
      }
  }
}

fn rename_expr(table: List<rename>, expr: expr) -> expr {
  match expr {
    Name(name) -> Name(rename_name(table, name)),
    Call(callee, args) -> Call(rename_name(table, callee), args),
    Let(name, value, body) -> {
      let renamed = rename_name(table, name);
      Let(renamed, rename_expr(table, value), rename_expr([rename { from: name, to: renamed }], body))
    }
  }
}

fn render(expr: expr) -> String {
  match expr {
    Name(name) -> name,
    Call(callee, args) ->
      match args {
        [] -> string_concat(callee, "()"),
        [Name(name), .._] -> string_concat(callee, string_concat("(", string_concat(name, ")"))),
        _ -> string_concat(callee, "(?)")
      },
    Let(name, value, body) -> string_concat("let ", string_concat(name, string_concat(" = ", string_concat(render(value), string_concat("; ", render(body))))))
  }
}

fn main() {
  let table = [rename { from: "x", to: "x$1" }, rename { from: "print", to: "println" }];
  let expr = Let("x", Name("input"), Call("print", [Name("x")]));
  println(render(rename_expr(table, expr)))
}
