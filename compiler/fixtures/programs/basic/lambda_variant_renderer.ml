type token =
  | KwFn
  | Ident(String)
  | IntLit(i64)

fn make_renderer(prefix: String) {
  fn(token: token) {
    match token {
      KwFn -> string_concat(prefix, "fn"),
      Ident(name) -> string_concat(prefix, name),
      IntLit(_) -> string_concat(prefix, "int")
    }
  }
}

fn main() {
  let render = make_renderer("tok:");
  println(render(KwFn));
  println(render(Ident("answer")));
  println(render(IntLit(42)))
}
