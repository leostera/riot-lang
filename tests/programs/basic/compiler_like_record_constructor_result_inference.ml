type typ = TInt | TString | TVar(String) | TRecord(String, List<typ>)
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
    TRecord(name, args) -> TRecord(name, substitute_list(args, substitutions))
  }
}

fn substitute_list(types: List<typ>, substitutions: List<subst>) -> List<typ> {
  match types {
    [] -> [],
    [type_, ..rest] -> [substitute(type_, substitutions), ..substitute_list(rest, substitutions)]
  }
}

fn bind_field_type_arg(field: field, actual: typ) -> subst {
  match field.type_ {
    TVar(name) -> subst { name: name, type_: actual },
    TInt -> subst { name: "_", type_: actual },
    TString -> subst { name: "_", type_: actual },
    TRecord(name, _) -> subst { name: name, type_: actual }
  }
}

fn infer_record_result(shape: record_shape, actual_field_type: typ) -> typ {
  match shape.fields {
    [] -> TRecord(shape.name, []),
    [field, .._] -> {
      let substitutions = [bind_field_type_arg(field, actual_field_type)];
      TRecord(shape.name, substitute_list(record_type_args(shape.type_params), substitutions))
    }
  }
}

fn record_type_args(params: List<String>) -> List<typ> {
  match params {
    [] -> [],
    [param, ..rest] -> [TVar(param), ..record_type_args(rest)]
  }
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
    TRecord(name, args) -> string_concat(name, string_concat("<", string_concat(render_args(args), ">")))
  }
}

fn main() {
  let box_shape = record_shape { name: "box", type_params: ["a"], fields: [field { name: "value", type_: TVar("a") }] };
  let inferred = infer_record_result(box_shape, TInt);
  dbg(string_concat("box literal infers ", render_type(inferred)))
}
