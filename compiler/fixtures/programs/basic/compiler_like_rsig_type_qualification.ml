type typ = TInt | TString | TVar(String) | TList(typ) | TTuple(List<typ>) | TRecord(String, List<typ>) | TVariant(String, List<typ>)

type field = { name: String, type_: typ }
type imported_record = { module_name: String, record_name: String, fields: List<field> }

fn is_prelude_type(name: String) -> bool {
  name == "Option"
}

fn already_qualified(name: String) -> bool {
  name == "Syntax.token" || name == "Syntax.span" || name == "Syntax.box"
}

fn qualify_name(module_name: String, name: String) -> String {
  if is_prelude_type(name) { name } else { if already_qualified(name) { name } else { string_concat(module_name, string_concat(".", name)) } }
}

fn qualify_type(module_name: String, type_: typ) -> typ {
  match type_ {
    TInt -> TInt,
    TString -> TString,
    TVar(name) -> TVar(name),
    TList(item) -> TList(qualify_type(module_name, item)),
    TTuple(items) -> TTuple(qualify_types(module_name, items)),
    TRecord(name, args) -> TRecord(qualify_name(module_name, name), qualify_types(module_name, args)),
    TVariant(name, args) -> TVariant(qualify_name(module_name, name), qualify_types(module_name, args))
  }
}

fn qualify_types(module_name: String, types: List<typ>) -> List<typ> {
  match types {
    [] -> [],
    [type_, ..rest] -> [qualify_type(module_name, type_), ..qualify_types(module_name, rest)]
  }
}

fn qualify_fields(module_name: String, fields: List<field>) -> List<field> {
  match fields {
    [] -> [],
    [field { name: name, type_: type_ }, ..rest] -> [field { name: name, type_: qualify_type(module_name, type_) }, ..qualify_fields(module_name, rest)]
  }
}

fn render_types(types: List<typ>) -> String {
  match types {
    [] -> "",
    [type_] -> render_type(type_),
    [type_, ..rest] -> string_concat(render_type(type_), string_concat(",", render_types(rest)))
  }
}

fn render_type(type_: typ) -> String {
  match type_ {
    TInt -> "i64",
    TString -> "String",
    TVar(name) -> name,
    TList(item) -> string_concat("List<", string_concat(render_type(item), ">")),
    TTuple(items) -> string_concat("(", string_concat(render_types(items), ")")),
    TRecord(name, args) -> if args == [] { name } else { string_concat(name, string_concat("<", string_concat(render_types(args), ">"))) },
    TVariant(name, args) -> if args == [] { name } else { string_concat(name, string_concat("<", string_concat(render_types(args), ">"))) }
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
  let provider = imported_record {
    module_name: "Syntax",
    record_name: "entry",
    fields: [
      field { name: "head", type_: TVariant("token", []) },
      field { name: "trail", type_: TRecord("box", [TVariant("token", []), TVariant("Option", [TString])]) },
      field { name: "pair", type_: TTuple([TVariant("token", []), TList(TRecord("span", []))]) },
      field { name: "forwarded", type_: TVariant("Syntax.token", []) }
    ]
  };
  dbg(render_fields(qualify_fields(provider.module_name, provider.fields)))
}
