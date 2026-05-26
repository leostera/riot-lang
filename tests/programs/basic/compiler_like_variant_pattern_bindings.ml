type typ = TInt | TString | TVar(String)
type subst = { name: String, type_: typ }
type binding = { name: String, type_: typ }
type pattern = PBind(String) | PConstructor(String, List<pattern>)
type constructor = { name: String, payload: List<typ> }

fn substitute(type_: typ, substitutions: List<subst>) -> typ {
  match type_ {
    TInt -> TInt,
    TString -> TString,
    TVar(name) -> lookup_subst(name, substitutions)
  }
}

fn lookup_subst(name: String, substitutions: List<subst>) -> typ {
  match substitutions {
    [] -> TVar(name),
    [subst { name: key, type_: value }, ..rest] -> if key == name { value } else { lookup_subst(name, rest) }
  }
}

fn instantiate_payload(payload: List<typ>, substitutions: List<subst>) -> List<typ> {
  match payload {
    [] -> [],
    [type_, ..rest] -> [substitute(type_, substitutions), ..instantiate_payload(rest, substitutions)]
  }
}

fn bind_pattern(pattern: pattern, expected: typ, payload_types: List<typ>) -> List<binding> {
  match pattern {
    PBind(name) -> [binding { name: name, type_: expected }],
    PConstructor(_, payload) -> bind_payload(payload, payload_types)
  }
}

fn bind_payload(patterns: List<pattern>, payload_types: List<typ>) -> List<binding> {
  match patterns {
    [] -> [],
    [pattern, ..rest] -> match payload_types {
      [] -> [],
      [type_, ..type_rest] -> append_bindings(bind_pattern(pattern, type_, []), bind_payload(rest, type_rest))
    }
  }
}

fn append_bindings(left: List<binding>, right: List<binding>) -> List<binding> {
  match left {
    [] -> right,
    [item, ..rest] -> [item, ..append_bindings(rest, right)]
  }
}

fn render_type(type_: typ) -> String {
  match type_ {
    TInt -> "i64",
    TString -> "String",
    TVar(name) -> name
  }
}

fn render_bindings(bindings: List<binding>) -> String {
  match bindings {
    [] -> "ok",
    [binding { name: name, type_: type_ }] -> string_concat(name, string_concat(":", render_type(type_))),
    [binding { name: name, type_: type_ }, ..rest] -> string_concat(name, string_concat(":", string_concat(render_type(type_), string_concat(",", render_bindings(rest)))))
  }
}

fn main() {
  let constructor = constructor { name: "Pair", payload: [TVar("a"), TString] };
  let payload = instantiate_payload(constructor.payload, [subst { name: "a", type_: TInt }]);
  let pattern = PConstructor("Pair", [PBind("value"), PBind("label")]);
  dbg(render_bindings(bind_pattern(pattern, TVar("pair"), payload)))
}
