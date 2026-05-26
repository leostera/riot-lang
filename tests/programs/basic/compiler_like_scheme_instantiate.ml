type typ =
  | TVar(String)
  | TInt
  | TString
  | TList(typ)
  | TFun(typ, typ)

type scheme = { vars: List<String>, body: typ }

type binding = { name: String, scheme: scheme }

fn contains_name(names: List<String>, wanted: String) -> bool {
  match names {
    [] -> false,
    [name, ..rest] ->
      if name == wanted {
        true
      } else {
        contains_name(rest, wanted)
      }
  }
}

fn suffix_type(typ: typ, suffix: String, quantified: List<String>) -> typ {
  match typ {
    TVar(name) ->
      if contains_name(quantified, name) {
        TVar(string_concat(name, suffix))
      } else {
        TVar(name)
      },
    TInt -> TInt,
    TString -> TString,
    TList(item) -> TList(suffix_type(item, suffix, quantified)),
    TFun(arg, result) -> TFun(suffix_type(arg, suffix, quantified), suffix_type(result, suffix, quantified))
  }
}

fn instantiate(scheme: scheme, suffix: String) -> typ {
  suffix_type(scheme.body, suffix, scheme.vars)
}

fn render(typ: typ) -> String {
  match typ {
    TVar(name) -> name,
    TInt -> "Int",
    TString -> "String",
    TList(item) -> string_concat("List<", string_concat(render(item), ">")),
    TFun(arg, result) -> string_concat(render(arg), string_concat(" -> ", render(result)))
  }
}

fn main() {
  let identity = scheme { vars: ["a"], body: TFun(TVar("a"), TVar("a")) };
  let map_list = scheme { vars: ["a", "b"], body: TFun(TFun(TVar("a"), TVar("b")), TFun(TList(TVar("a")), TList(TVar("b")))) };
  println(render(instantiate(identity, "#1")));
  println(render(instantiate(identity, "#2")));
  println(render(instantiate(map_list, "#3")))
}
