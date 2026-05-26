type typ =
  | TInt
  | TString
  | TRecord(String, List<String>)

type expr =
  | IntLit
  | StringLit
  | RecordLit(String, List<String>)
  | Field(expr, String)

type check = { typ: typ, diagnostics: List<String> }

fn contains(name: String, fields: List<String>) -> bool {
  match fields {
    [] -> false,
    [head, ..tail] -> if head == name { true } else { contains(name, tail) }
  }
}

fn append(left: List<String>, right: List<String>) -> List<String> {
  match left {
    [] -> right,
    [head, ..tail] -> [head, ..append(tail, right)]
  }
}

fn render_type(typ: typ) -> String {
  match typ {
    TInt -> "Int",
    TString -> "String",
    TRecord(name, _) -> string_concat("Record ", name)
  }
}

fn check_expr(expr: expr) -> check {
  match expr {
    IntLit -> check { typ: TInt, diagnostics: [] },
    StringLit -> check { typ: TString, diagnostics: [] },
    RecordLit(name, fields) -> check { typ: TRecord(name, fields), diagnostics: [] },
    Field(base, field) -> {
      let base_check = check_expr(base);
      match base_check.typ {
        TRecord(_, fields) -> if contains(field, fields) {
          check { typ: TString, diagnostics: base_check.diagnostics }
        } else {
          check { typ: TString, diagnostics: [string_concat("unknown field ", field), ..base_check.diagnostics] }
        },
        other -> check { typ: TString, diagnostics: [string_concat("non-record field base: ", render_type(other)), ..base_check.diagnostics] }
      }
    }
  }
}

fn join(items: List<String>) -> String {
  match items {
    [] -> "",
    [item] -> item,
    [item, ..rest] -> string_concat(item, string_concat("; ", join(rest)))
  }
}

fn main() {
  let missing = check_expr(Field(RecordLit("token", ["kind", "span"]), "text"));
  let scalar = check_expr(Field(IntLit, "field"));
  println(join(append(missing.diagnostics, scalar.diagnostics)))
}
