type typ =
  | TVar(String)
  | TInt
  | TList(typ)
  | TFun(typ, typ)

type subst = { name: String, typ: typ }

type found =
  | Missing
  | Found(typ)

fn find_subst(name: String, substitutions: List<subst>) -> found {
  match substitutions {
    [] -> Missing,
    [entry, ..rest] ->
      if entry.name == name {
        Found(entry.typ)
      } else {
        find_subst(name, rest)
      }
  }
}

fn apply_subst(typ: typ, substitutions: List<subst>) -> typ {
  match typ {
    TVar(name) ->
      match find_subst(name, substitutions) {
        Missing -> TVar(name),
        Found(replacement) -> apply_subst(replacement, substitutions)
      },
    TInt -> TInt,
    TList(item) -> TList(apply_subst(item, substitutions)),
    TFun(arg, result) -> TFun(apply_subst(arg, substitutions), apply_subst(result, substitutions))
  }
}

fn render(typ: typ) -> String {
  match typ {
    TVar(name) -> name,
    TInt -> "Int",
    TList(item) -> string_concat("List<", string_concat(render(item), ">")),
    TFun(arg, result) -> string_concat(render(arg), string_concat(" -> ", render(result)))
  }
}

fn main() {
  let substitutions = [subst { name: "a", typ: TInt }, subst { name: "b", typ: TList(TVar("a")) }];
  let typ = TFun(TVar("b"), TList(TVar("c")));
  println(render(apply_subst(typ, substitutions)))
}
