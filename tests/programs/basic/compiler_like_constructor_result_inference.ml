type typ = TInt | TString | TVar(String) | TRecord(String, List<typ>) | TVariant(String, List<typ>)
type subst = { name: String, type_: typ }
type constructor_shape = { module_name: String, name: String, type_params: List<String>, payload: typ, result: typ }

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
    TRecord(name, args) -> TRecord(name, substitute_list(args, substitutions)),
    TVariant(name, args) -> TVariant(name, substitute_list(args, substitutions))
  }
}

fn substitute_list(types: List<typ>, substitutions: List<subst>) -> List<typ> {
  match types {
    [] -> [],
    [type_, ..rest] -> [substitute(type_, substitutions), ..substitute_list(rest, substitutions)]
  }
}

fn bind_constructor_type_arg(parameter: String, payload_type: typ) -> List<subst> {
  [subst { name: parameter, type_: payload_type }]
}

fn infer_constructor_result(constructor: constructor_shape, payload_type: typ) -> typ {
  let substitutions = bind_constructor_type_arg("a", payload_type);
  substitute(constructor.result, substitutions)
}

fn render_args(args: List<typ>) -> String {
  match args {
    [] -> "",
    [arg] -> render_type(arg),
    [arg, ..rest] -> string_concat(render_type(arg), string_concat(", ", render_args(rest)))
  }
}

fn render_type(type_: typ) -> String {
  match type_ {
    TInt -> "i64",
    TString -> "String",
    TVar(name) -> name,
    TRecord(name, args) -> string_concat(name, string_concat("<", string_concat(render_args(args), ">"))),
    TVariant(name, args) -> string_concat(name, string_concat("<", string_concat(render_args(args), ">")))
  }
}

fn main() {
  let some = constructor_shape { module_name: "Options", name: "Some", type_params: ["a"], payload: TVar("a"), result: TVariant("Options.option", [TVar("a")]) };
  let payload = TRecord("Boxes.box", [TInt]);
  let result = infer_constructor_result(some, payload);
  dbg(string_concat("Options.Some payload infers ", render_type(result)))
}
