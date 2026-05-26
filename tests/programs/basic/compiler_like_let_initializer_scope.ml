type binding = { name: String, slot: String }

type expr =
  | Name(String)
  | Let(String, String, expr, expr)
  | Pair(expr, expr)

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

fn render(expr: expr, scope: List<binding>) -> String {
  match expr {
    Name(name) -> resolve(name, scope),
    Pair(left, right) -> string_concat(render(left, scope), string_concat(",", render(right, scope))),
    Let(name, slot, value, body) ->
      string_concat(render(value, scope), string_concat(" -> ", render(body, [binding { name: name, slot: slot }, ..scope])))
  }
}

fn main() {
  let scope = [binding { name: "value", slot: "outer" }, binding { name: "other", slot: "stable" }];
  let shadow = Let("value", "inner", Name("value"), Pair(Name("value"), Name("other")));
  let missing = Let("value", "inner", Name("missing"), Name("value"));
  println(string_concat(render(shadow, scope), string_concat("; ", render(missing, scope))))
}
