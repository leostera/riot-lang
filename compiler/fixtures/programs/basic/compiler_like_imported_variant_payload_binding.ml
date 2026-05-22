type typ = TInt | TString | TVar(String) | TVariant(String, List<typ>)
type subst = { name: String, type_: typ }
type constructor_shape = { module_name: String, name: String, type_params: List<String>, payload: typ }
type binding = { name: String, type_: typ }

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
    TVariant(name, args) -> TVariant(name, substitute_list(args, substitutions))
  }
}

fn substitute_list(types: List<typ>, substitutions: List<subst>) -> List<typ> {
  match types {
    [] -> [],
    [type_, ..rest] -> [substitute(type_, substitutions), ..substitute_list(rest, substitutions)]
  }
}

fn bind_payload(shape: constructor_shape, substitutions: List<subst>, binding_name: String) -> binding {
  binding { name: binding_name, type_: substitute(shape.payload, substitutions) }
}

fn qualify_constructor(shape: constructor_shape) -> String {
  string_concat(shape.module_name, string_concat(".", shape.name))
}

fn render_type(type_: typ) -> String {
  match type_ {
    TInt -> "i64",
    TString -> "String",
    TVar(name) -> name,
    TVariant(name, _) -> name
  }
}

fn render_binding(binding: binding) -> String {
  string_concat(binding.name, string_concat(":", render_type(binding.type_)))
}

fn main() {
  let some = constructor_shape { module_name: "Options", name: "Some", type_params: ["a"], payload: TVar("a") };
  let binding = bind_payload(some, [subst { name: "a", type_: TInt }], "value");
  dbg(string_concat(qualify_constructor(some), string_concat(" binds ", render_binding(binding))))
}
