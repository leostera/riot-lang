type expr =
  | Name(String)
  | Call(String, List<expr>)
  | Let(String, expr, expr)

fn contains_free_arg(name: String, args: List<expr>) -> bool {
  match args {
    [] -> false,
    [arg, ..rest] ->
      if contains_free_expr(name, arg) {
        true
      } else {
        contains_free_arg(name, rest)
      }
  }
}

fn contains_free_expr(name: String, expr: expr) -> bool {
  match expr {
    Name(found) -> found == name,
    Call(callee, args) ->
      if callee == name {
        true
      } else {
        contains_free_arg(name, args)
      },
    Let(binding, value, body) ->
      if contains_free_expr(name, value) {
        true
      } else {
        if binding == name {
          false
        } else {
          contains_free_expr(name, body)
        }
      }
  }
}

fn main() {
  let expr = Let("tmp", Call("load", [Name("input")]), Call("print", [Name("tmp"), Name("suffix")]));
  dbg(contains_free_expr("input", expr));
  dbg(contains_free_expr("tmp", expr));
  dbg(contains_free_expr("suffix", expr))
}
