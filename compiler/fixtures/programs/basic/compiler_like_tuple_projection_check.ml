type typ =
  | TInt
  | TString
  | TTuple(List<typ>)

type projection = { origin: String, base: typ, index: i64 }
type check = { result: typ, diagnostics: List<String> }

fn length(items: List<typ>) -> i64 {
  match items {
    [] -> 0,
    [_, ..tail] -> 1 + length(tail)
  }
}

fn get_or_default(items: List<typ>, index: i64, default: typ) -> typ {
  match items {
    [] -> default,
    [head, ..tail] -> if index == 0 { head } else { get_or_default(tail, index - 1, default) }
  }
}

fn render_type(typ: typ) -> String {
  match typ {
    TInt -> "Int",
    TString -> "String",
    TTuple(_) -> "Tuple"
  }
}

fn check_projection(projection: projection) -> check {
  match projection.base {
    TTuple(items) -> if projection.index < length(items) {
      check { result: get_or_default(items, projection.index, TString), diagnostics: [] }
    } else {
      check { result: TString, diagnostics: [string_concat(projection.origin, " tuple index out of bounds")] }
    },
    other -> check { result: TString, diagnostics: [string_concat(projection.origin, string_concat(" non-tuple base ", render_type(other)))] }
  }
}

fn append(left: List<String>, right: List<String>) -> List<String> {
  match left {
    [] -> right,
    [head, ..tail] -> [head, ..append(tail, right)]
  }
}

fn check_all(projections: List<projection>) -> List<String> {
  match projections {
    [] -> [],
    [head, ..tail] -> append(check_projection(head).diagnostics, check_all(tail))
  }
}

fn join(items: List<String>) -> String {
  match items {
    [] -> "ok",
    [item] -> item,
    [item, ..rest] -> string_concat(item, string_concat("; ", join(rest)))
  }
}

fn main() {
  let projections = [
    projection { origin: "ok", base: TTuple([TInt, TString]), index: 1 },
    projection { origin: "bad-index", base: TTuple([TInt, TString]), index: 2 },
    projection { origin: "bad-base", base: TInt, index: 0 }
  ];
  println(join(check_all(projections)))
}
