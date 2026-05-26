type typ = TInt | TString | TVar(String) | TRecord(String, List<typ>) | TVariant(String, List<typ>)
type subst = { name: String, type_: typ }
type field = { name: String, type_: typ }
type record_shape = { module_name: String, name: String, type_params: List<String>, fields: List<field> }
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

fn instantiate_fields(fields: List<field>, substitutions: List<subst>) -> List<field> {
  match fields {
    [] -> [],
    [field { name: name, type_: type_ }, ..rest] -> [field { name: name, type_: substitute(type_, substitutions) }, ..instantiate_fields(rest, substitutions)]
  }
}

fn find_field(name: String, fields: List<field>) -> typ {
  match fields {
    [] -> TString,
    [field { name: key, type_: type_ }, ..rest] -> if key == name { type_ } else { find_field(name, rest) }
  }
}

fn bind_nested_value(option_shape: constructor_shape, record_shape: record_shape, substitutions: List<subst>) -> binding {
  let payload_type = substitute(option_shape.payload, substitutions);
  match payload_type {
    TRecord(_, record_args) -> {
      let fields = instantiate_fields(record_shape.fields, [subst { name: "box_a", type_: find_field_type_arg(record_args) }]);
      binding { name: "value", type_: find_field("value", fields) }
    },
    TInt -> binding { name: "value", type_: TInt },
    TString -> binding { name: "value", type_: TString },
    TVar(name) -> binding { name: "value", type_: TVar(name) },
    TVariant(name, args) -> binding { name: "value", type_: TVariant(name, args) }
  }
}

fn find_field_type_arg(args: List<typ>) -> typ {
  match args {
    [] -> TString,
    [type_] -> type_,
    [type_, .._] -> type_
  }
}

fn render_type(type_: typ) -> String {
  match type_ {
    TInt -> "i64",
    TString -> "String",
    TVar(name) -> name,
    TRecord(name, _) -> name,
    TVariant(name, _) -> name
  }
}

fn render_binding(binding: binding) -> String {
  string_concat(binding.name, string_concat(":", render_type(binding.type_)))
}

fn main() {
  let some = constructor_shape { module_name: "Options", name: "Some", type_params: ["a"], payload: TVar("a") };
  let box = record_shape { module_name: "Boxes", name: "box", type_params: ["box_a"], fields: [field { name: "value", type_: TVar("box_a") }] };
  let nested = bind_nested_value(some, box, [subst { name: "a", type_: TRecord("Boxes.box", [TInt]) }]);
  dbg(string_concat("Options.Some(Boxes.box) binds ", render_binding(nested)))
}
