type binding = { name: String, slot: String }

type expr =
  | Name(String)
  | Let(String, String, expr)
  | Tuple(expr, expr)

fn resolve(name: String, scope: List<binding>) -> String {
  match scope {
    [] -> "missing",
    [entry, ..rest] ->
      if entry.name == name {
        entry.slot
      } else {
        resolve(name, rest)
      }
  }
}

fn resolve_expr(expr: expr, scope: List<binding>) -> String {
  match expr {
    Name(name) -> resolve(name, scope),
    Tuple(left, right) -> string_concat(resolve_expr(left, scope), string_concat(",", resolve_expr(right, scope))),
    Let(name, slot, body) -> resolve_expr(body, [binding { name: name, slot: slot }, ..scope])
  }
}

fn main() {
  let scope = [binding { name: "x", slot: "outer" }, binding { name: "y", slot: "param" }];
  let expr = Let("x", "inner", Tuple(Name("x"), Name("y")));
  println(resolve_expr(expr, scope))
}
