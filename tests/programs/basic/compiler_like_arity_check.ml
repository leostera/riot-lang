type expr =
  | Name(String)
  | Call(String, List<expr>)
  | Pair(expr, expr)

type signature = { name: String, arity: i64 }

type diagnostic = { message: String }

fn lookup_arity(name: String, signatures: List<signature>) -> i64 {
  match signatures {
    [] -> -1,
    [signature, ..rest] ->
      if signature.name == name {
        signature.arity
      } else {
        lookup_arity(name, rest)
      }
  }
}

fn len_exprs(items: List<expr>) -> i64 {
  match items {
    [] -> 0,
    [_, ..rest] -> 1 + len_exprs(rest)
  }
}

fn append(left: List<diagnostic>, right: List<diagnostic>) -> List<diagnostic> {
  match left {
    [] -> right,
    [item, ..rest] -> [item, ..append(rest, right)]
  }
}

fn check_args(args: List<expr>, signatures: List<signature>) -> List<diagnostic> {
  match args {
    [] -> [],
    [arg, ..rest] -> append(check_expr(arg, signatures), check_args(rest, signatures))
  }
}

fn check_call(name: String, args: List<expr>, signatures: List<signature>) -> List<diagnostic> {
  let expected = lookup_arity(name, signatures);
  let nested = check_args(args, signatures);
  if expected == len_exprs(args) {
    nested
  } else {
    [diagnostic { message: string_concat("arity mismatch: ", name) }, ..nested]
  }
}

fn check_expr(expr: expr, signatures: List<signature>) -> List<diagnostic> {
  match expr {
    Name(_) -> [],
    Pair(left, right) -> append(check_expr(left, signatures), check_expr(right, signatures)),
    Call(name, args) -> check_call(name, args, signatures)
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
  let signatures = [signature { name: "print", arity: 1 }, signature { name: "join", arity: 2 }];
  let expr = Pair(Call("print", []), Call("join", [Name("left"), Name("right"), Name("extra")]));
  println(render_diagnostics(check_expr(expr, signatures)))
}
