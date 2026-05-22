type field_decl = { name: String, typ: String }
type record_shape = { name: String, fields: List<field_decl> }
type field_value = { name: String, typ: String }
type literal = { record: String, fields: List<field_value> }

type diagnostic =
  | Missing(String)
  | Unknown(String)
  | Mismatch(String, String, String)

fn find_decl(name: String, fields: List<field_decl>) -> String {
  match fields {
    [] -> "",
    [field, ..rest] -> if field.name == name { field.typ } else { find_decl(name, rest) }
  }
}

fn has_value(name: String, fields: List<field_value>) -> bool {
  match fields {
    [] -> false,
    [field, ..rest] -> if field.name == name { true } else { has_value(name, rest) }
  }
}

fn check_missing(decls: List<field_decl>, values: List<field_value>) -> List<diagnostic> {
  match decls {
    [] -> [],
    [decl, ..rest] -> if has_value(decl.name, values) {
      check_missing(rest, values)
    } else {
      [Missing(decl.name), ..check_missing(rest, values)]
    }
  }
}

fn check_values(decls: List<field_decl>, values: List<field_value>) -> List<diagnostic> {
  match values {
    [] -> [],
    [value, ..rest] -> {
      let expected = find_decl(value.name, decls);
      if expected == "" {
        [Unknown(value.name), ..check_values(decls, rest)]
      } else if expected == value.typ {
        check_values(decls, rest)
      } else {
        [Mismatch(value.name, expected, value.typ), ..check_values(decls, rest)]
      }
    }
  }
}

fn append(left: List<diagnostic>, right: List<diagnostic>) -> List<diagnostic> {
  match left {
    [] -> right,
    [head, ..tail] -> [head, ..append(tail, right)]
  }
}

fn check_literal(shape: record_shape, literal: literal) -> List<diagnostic> {
  append(check_missing(shape.fields, literal.fields), check_values(shape.fields, literal.fields))
}

fn render(diagnostic: diagnostic) -> String {
  match diagnostic {
    Missing(name) -> string_concat("missing ", name),
    Unknown(name) -> string_concat("unknown ", name),
    Mismatch(name, expected, actual) -> string_concat(name, string_concat(" expected ", string_concat(expected, string_concat(" got ", actual))))
  }
}

fn join(diagnostics: List<diagnostic>) -> String {
  match diagnostics {
    [] -> "ok",
    [diagnostic] -> render(diagnostic),
    [diagnostic, ..rest] -> string_concat(render(diagnostic), string_concat("; ", join(rest)))
  }
}

fn main() {
  let shape = record_shape { name: "token", fields: [field_decl { name: "kind", typ: "String" }, field_decl { name: "span", typ: "Span" }] };
  let literal = literal { record: "token", fields: [field_value { name: "kind", typ: "Bool" }, field_value { name: "text", typ: "String" }] };
  println(join(check_literal(shape, literal)))
}
