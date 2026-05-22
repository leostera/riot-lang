type typ = TInt | TString | TVar(String) | TTuple(List<typ>) | TVariant(String, List<typ>)
type subst = { name: String, type_: typ }
type constructor_shape = { name: String, type_params: List<String>, payload: List<typ> }

fn lookup(name: String, substitutions: List<subst>) -> typ {
  match substitutions {
    [] -> TVar(name),
    [subst { name: key, type_: value }, ..rest] -> if key == name { value } else { lookup(name, rest) }
  }
}

fn substitute(type_: typ, substitutions: List<subst>) -> typ {
  match type_ {
    TInt -> TInt,
    TString -> TString,
    TVar(name) -> lookup(name, substitutions),
    TTuple(items) -> TTuple(substitute_list(items, substitutions)),
    TVariant(name, args) -> TVariant(name, substitute_list(args, substitutions))
  }
}

fn substitute_list(types: List<typ>, substitutions: List<subst>) -> List<typ> {
  match types {
    [] -> [],
    [type_, ..rest] -> [substitute(type_, substitutions), ..substitute_list(rest, substitutions)]
  }
}

fn payload_types(constructor: constructor_shape, substitutions: List<subst>) -> List<typ> {
  substitute_list(constructor.payload, substitutions)
}

fn render(type_: typ) -> String {
  match type_ {
    TInt -> "i64",
    TString -> "String",
    TVar(name) -> name,
    TTuple(items) -> string_concat("(", string_concat(join(items), ")")),
    TVariant(name, _) -> name
  }
}

fn join(types: List<typ>) -> String {
  match types {
    [] -> "",
    [type_] -> render(type_),
    [type_, ..rest] -> string_concat(render(type_), string_concat(",", join(rest)))
  }
}

fn main() {
  let some = constructor_shape { name: "Some", type_params: ["a"], payload: [TVar("a")] };
  let pair = constructor_shape { name: "Pair", type_params: ["a", "b"], payload: [TTuple([TVar("a"), TVar("b")])] };
  let substitutions = [subst { name: "a", type_: TInt }, subst { name: "b", type_: TString }];
  dbg(string_concat(join(payload_types(some, substitutions)), string_concat(";", join(payload_types(pair, substitutions)))))
}
