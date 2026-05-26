type typ = TInt | TString | TVar(String) | TRecord(String, List<typ>) | TUnknown
type subst = { name: String, type_: typ }
type field = { name: String, type_: typ }
type record_shape = { name: String, type_params: List<String>, fields: List<field> }
type projection = { base: typ, field: String }

fn lookup_var(name: String, substitutions: List<subst>) -> typ {
  match substitutions {
    [] -> TVar(name),
    [subst { name: key, type_: value }, ..rest] -> if key == name { value } else { lookup_var(name, rest) }
  }
}

fn substitute(type_: typ, substitutions: List<subst>) -> typ {
  match type_ {
    TInt -> TInt,
    TString -> TString,
    TVar(name) -> lookup_var(name, substitutions),
    TRecord(name, args) -> TRecord(name, substitute_list(args, substitutions)),
    TUnknown -> TUnknown
  }
}

fn substitute_list(types: List<typ>, substitutions: List<subst>) -> List<typ> {
  match types {
    [] -> [],
    [type_, ..rest] -> [substitute(type_, substitutions), ..substitute_list(rest, substitutions)]
  }
}

fn bind_params(params: List<String>, args: List<typ>) -> List<subst> {
  match (params, args) {
    ([], []) -> [],
    ([param, ..rest_params], [arg, ..rest_args]) -> [subst { name: param, type_: arg }, ..bind_params(rest_params, rest_args)],
    _ -> []
  }
}

fn find_field(name: String, fields: List<field>) -> typ {
  match fields {
    [] -> TUnknown,
    [field { name: field_name, type_: type_ }, ..rest] -> if field_name == name { type_ } else { find_field(name, rest) }
  }
}

fn project(shape: record_shape, projection: projection) -> typ {
  match projection.base {
    TRecord(record_name, args) -> if record_name == shape.name {
      let substitutions = bind_params(shape.type_params, args);
      substitute(find_field(projection.field, shape.fields), substitutions)
    } else {
      TUnknown
    },
    _ -> TUnknown
  }
}

fn render_type(type_: typ) -> String {
  match type_ {
    TInt -> "i64",
    TString -> "String",
    TVar(name) -> name,
    TRecord(name, _) -> name,
    TUnknown -> "unknown"
  }
}

fn render_projection(shape: record_shape, projection: projection) -> String {
  render_type(project(shape, projection))
}

fn main() {
  let shape = record_shape { name: "Boxes.box", type_params: ["a"], fields: [field { name: "value", type_: TVar("a") }] };
  let local = projection { base: TRecord("Boxes.box", [TInt]), field: "value" };
  let missing = projection { base: TRecord("Boxes.box", [TString]), field: "missing" };
  dbg(string_concat(render_projection(shape, local), string_concat(";", render_projection(shape, missing))))
}
