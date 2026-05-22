type typ = TInt | TString | TVar(String)
type subst = { name: String, type_: typ }
type field_type = { name: String, type_: typ }
type binding = { name: String, type_: typ }
type pattern = PBind(String) | PRecord(List<field_pattern>)
type field_pattern = { name: String, pattern: pattern }

fn lookup_type(name: String, fields: List<field_type>) -> typ {
  match fields {
    [] -> TVar("unknown"),
    [field_type { name: field, type_: type_ }, ..rest] -> if field == name { type_ } else { lookup_type(name, rest) }
  }
}

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

fn instantiate_fields(fields: List<field_type>, substitutions: List<subst>) -> List<field_type> {
  match fields {
    [] -> [],
    [field_type { name: name, type_: type_ }, ..rest] -> [field_type { name: name, type_: substitute(type_, substitutions) }, ..instantiate_fields(rest, substitutions)]
  }
}

fn bind_pattern(pattern: pattern, expected: typ, fields: List<field_type>) -> List<binding> {
  match pattern {
    PBind(name) -> [binding { name: name, type_: expected }],
    PRecord(items) -> bind_fields(items, fields)
  }
}

fn bind_fields(patterns: List<field_pattern>, fields: List<field_type>) -> List<binding> {
  match patterns {
    [] -> [],
    [field_pattern { name: name, pattern: pattern }, ..rest] -> append_bindings(bind_pattern(pattern, lookup_type(name, fields), fields), bind_fields(rest, fields))
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
  let generic_fields = [field_type { name: "value", type_: TVar("a") }, field_type { name: "label", type_: TString }];
  let fields = instantiate_fields(generic_fields, [subst { name: "a", type_: TInt }]);
  let pattern = PRecord([field_pattern { name: "value", pattern: PBind("value") }, field_pattern { name: "label", pattern: PBind("label") }]);
  dbg(render_bindings(bind_pattern(pattern, TVar("box"), fields)))
}
