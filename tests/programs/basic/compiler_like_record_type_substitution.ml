type typ = TInt | TString | TVar(String) | TList(typ) | TRecord(String, List<typ>)
type subst = { name: String, type_: typ }
type field = { name: String, type_: typ }
type record_shape = { name: String, type_params: List<String>, fields: List<field> }

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
    TList(item) -> TList(substitute(item, substitutions)),
    TRecord(name, args) -> TRecord(name, substitute_list(args, substitutions))
  }
}

fn substitute_list(types: List<typ>, substitutions: List<subst>) -> List<typ> {
  match types {
    [] -> [],
    [type_, ..rest] -> [substitute(type_, substitutions), ..substitute_list(rest, substitutions)]
  }
}

fn instantiate_fields(fields: List<field>, substitutions: List<subst>) -> List<field> {
  match fields {
    [] -> [],
    [field { name: name, type_: type_ }, ..rest] -> [field { name: name, type_: substitute(type_, substitutions) }, ..instantiate_fields(rest, substitutions)]
  }
}

fn render_type(type_: typ) -> String {
  match type_ {
    TInt -> "i64",
    TString -> "String",
    TVar(name) -> name,
    TList(item) -> string_concat("List<", string_concat(render_type(item), ">")),
    TRecord(name, _) -> name
  }
}

fn render_fields(fields: List<field>) -> String {
  match fields {
    [] -> "ok",
    [field { name: name, type_: type_ }] -> string_concat(name, string_concat(":", render_type(type_))),
    [field { name: name, type_: type_ }, ..rest] -> string_concat(name, string_concat(":", string_concat(render_type(type_), string_concat(",", render_fields(rest)))))
  }
}

fn main() {
  let shape = record_shape { name: "box", type_params: ["a"], fields: [field { name: "value", type_: TVar("a") }, field { name: "history", type_: TList(TVar("a")) }] };
  let substitutions = [subst { name: "a", type_: TInt }];
  dbg(render_fields(instantiate_fields(shape.fields, substitutions)))
}
