type typ =
  | TVar(String)
  | TInt
  | TBool
  | TString
  | TList(typ)
  | TFun(typ, typ)

type binding = { name: String, typ: typ }
type scheme = { vars: List<String>, body: typ }
type env_entry = { name: String, scheme: scheme }

fn contains(name: String, names: List<String>) -> bool {
  match names {
    [] -> false,
    [head, ..tail] -> if head == name { true } else { contains(name, tail) }
  }
}

fn append(left: List<String>, right: List<String>) -> List<String> {
  match left {
    [] -> right,
    [head, ..tail] -> if contains(head, right) { append(tail, right) } else { [head, ..append(tail, right)] }
  }
}

fn free_type_vars(typ: typ) -> List<String> {
  match typ {
    TVar(name) -> [name],
    TInt -> [],
    TBool -> [],
    TString -> [],
    TList(item) -> free_type_vars(item),
    TFun(param, result) -> append(free_type_vars(param), free_type_vars(result))
  }
}

fn remove(names: List<String>, blocked: List<String>) -> List<String> {
  match names {
    [] -> [],
    [head, ..tail] -> if contains(head, blocked) { remove(tail, blocked) } else { [head, ..remove(tail, blocked)] }
  }
}

fn env_free_vars(env: List<env_entry>) -> List<String> {
  match env {
    [] -> [],
    [entry, ..rest] -> append(remove(free_type_vars(entry.scheme.body), entry.scheme.vars), env_free_vars(rest))
  }
}

fn generalize(env: List<env_entry>, typ: typ) -> scheme {
  scheme { vars: remove(free_type_vars(typ), env_free_vars(env)), body: typ }
}

fn render_names(names: List<String>) -> String {
  match names {
    [] -> "[]",
    [name] -> name,
    [name, ..rest] -> string_concat(name, string_concat(",", render_names(rest)))
  }
}

fn main() {
  let env = [env_entry { name: "id", scheme: scheme { vars: ["a"], body: TFun(TVar("a"), TVar("a")) } }];
  let inferred = TFun(TVar("a"), TFun(TVar("b"), TList(TVar("b"))));
  let generalized = generalize(env, inferred);
  println(string_concat("generalized vars: ", render_names(generalized.vars)))
}
