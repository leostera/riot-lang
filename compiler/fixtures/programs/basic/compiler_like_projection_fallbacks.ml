type typ = TInt | TString | TTuple(i64) | TRecord(List<String>)
type expr = EmptyMatch | TupleProjection(typ, i64) | RecordFieldProjection(List<String>, String)
type diagnostic = { message: String }

fn contains(name: String, fields: List<String>) -> bool {
  match fields {
    [] -> false,
    [field, ..rest] -> if field == name { true } else { contains(name, rest) }
  }
}

fn classify(expr: expr) -> diagnostic {
  match expr {
    EmptyMatch -> diagnostic { message: "empty match" },
    TupleProjection(TTuple(size), index) -> if index < size { diagnostic { message: "ok" } } else { diagnostic { message: "tuple projection is out of bounds" } },
    TupleProjection(_, _) -> diagnostic { message: "tuple projection on non-tuple value" },
    RecordFieldProjection(fields, name) -> if contains(name, fields) { diagnostic { message: "ok" } } else { diagnostic { message: "record literal has no such field" } }
  }
}

fn join_messages(items: List<expr>) -> String {
  match items {
    [] -> "done",
    [item] -> classify(item).message,
    [item, ..rest] -> string_concat(classify(item).message, string_concat(";", join_messages(rest)))
  }
}

fn main() {
  let cases = [
    EmptyMatch,
    TupleProjection(TTuple(2), 2),
    TupleProjection(TString, 0),
    RecordFieldProjection(["x", "y"], "z"),
    RecordFieldProjection(["x", "y"], "x")
  ];
  dbg(join_messages(cases))
}
