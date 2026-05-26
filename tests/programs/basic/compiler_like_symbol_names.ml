type expr =
  | Name(String)
  | Call(String, List<expr>)
  | Let(String, expr, expr)

type symbol = { name: String }

fn has_symbol(name: String, symbols: List<symbol>) -> bool {
  match symbols {
    [] -> false,
    [symbol, ..rest] ->
      if symbol.name == name {
        true
      } else {
        has_symbol(name, rest)
      }
  }
}

fn add_symbol(name: String, symbols: List<symbol>) -> List<symbol> {
  if has_symbol(name, symbols) {
    symbols
  } else {
    [symbol { name: name }, ..symbols]
  }
}

fn collect_arg_names(args: List<expr>, symbols: List<symbol>) -> List<symbol> {
  match args {
    [] -> symbols,
    [arg, ..rest] -> collect_arg_names(rest, collect_expr_names(arg, symbols))
  }
}

fn collect_expr_names(expr: expr, symbols: List<symbol>) -> List<symbol> {
  match expr {
    Name(name) -> add_symbol(name, symbols),
    Call(callee, args) -> collect_arg_names(args, add_symbol(callee, symbols)),
    Let(name, value, body) -> collect_expr_names(body, collect_expr_names(value, add_symbol(name, symbols)))
  }
}

fn render_symbols(symbols: List<symbol>) -> String {
  match symbols {
    [] -> "",
    [symbol, ..rest] ->
      match rest {
        [] -> symbol.name,
        _ -> string_concat(symbol.name, string_concat(",", render_symbols(rest)))
      }
  }
}

fn main() {
  let expr = Let("tmp", Call("load", [Name("input")]), Call("print", [Name("tmp"), Name("input")]));
  println(render_symbols(collect_expr_names(expr, [])))
}
