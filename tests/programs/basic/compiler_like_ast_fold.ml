type expr =
  | Int(i64)
  | Add(expr, expr)
  | Let(String, expr, expr)
  | Var(String)

type binding = Binding(String, i64)

fn lookup(env: List<binding>, name: String) -> i64 {
  match env {
    [] -> 0,
    [binding, ..rest] ->
      match binding {
        Binding(binding_name, value) ->
          if binding_name == name { value } else { lookup(rest, name) }
      }
  }
}

fn eval(env: List<binding>, expr: expr) -> i64 {
  match expr {
    Int(value) -> value,
    Add(left, right) -> eval(env, left) + eval(env, right),
    Let(name, value_expr, body) -> {
      let value = eval(env, value_expr);
      eval([Binding(name, value), ..env], body)
    },
    Var(name) -> lookup(env, name)
  }
}

fn main() {
  let program = Let("x", Int(40), Add(Var("x"), Int(2)));
  dbg(eval([], program))
}
