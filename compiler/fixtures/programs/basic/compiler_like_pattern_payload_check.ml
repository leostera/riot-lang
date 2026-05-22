type typ = TInt | TString | TBool | TTuple(List<typ>)
type pattern = PWildcard | PInt(i64) | PString(String) | PTuple(List<pattern>) | PConstructor(String, List<pattern>)
type diagnostic = { message: String }
type check = { matched: bool, diagnostics: List<diagnostic> }

fn typ_name(typ: typ) -> String {
  match typ {
    TInt -> "Int",
    TString -> "String",
    TBool -> "Bool",
    TTuple(_) -> "Tuple"
  }
}

fn check_pattern(pattern: pattern, expected: typ) -> check {
  match pattern {
    PWildcard -> check { matched: true, diagnostics: [] },
    PInt(_) -> match expected {
      TInt -> check { matched: true, diagnostics: [] },
      other -> check { matched: false, diagnostics: [diagnostic { message: string_concat("int pattern vs ", typ_name(other)) }] }
    },
    PString(_) -> match expected {
      TString -> check { matched: true, diagnostics: [] },
      other -> check { matched: false, diagnostics: [diagnostic { message: string_concat("string pattern vs ", typ_name(other)) }] }
    },
    PTuple(items) -> match expected {
      TTuple(types) -> check_pattern_list(items, types),
      other -> check { matched: false, diagnostics: [diagnostic { message: string_concat("tuple pattern vs ", typ_name(other)) }] }
    },
    PConstructor(name, payload) -> check_constructor_payload(name, payload)
  }
}

fn check_pattern_list(patterns: List<pattern>, types: List<typ>) -> check {
  match patterns {
    [] -> match types {
      [] -> check { matched: true, diagnostics: [] },
      _ -> check { matched: false, diagnostics: [diagnostic { message: "tuple arity" }] }
    },
    [pattern, ..rest_patterns] -> match types {
      [typ, ..rest_types] -> merge(check_pattern(pattern, typ), check_pattern_list(rest_patterns, rest_types)),
      [] -> check { matched: false, diagnostics: [diagnostic { message: "tuple arity" }] }
    }
  }
}

fn check_constructor_payload(name: String, payload: List<pattern>) -> check {
  match name {
    "Some" -> check_pattern_list(payload, [TInt]),
    "Pair" -> check_pattern_list(payload, [TString, TBool]),
    _ -> check { matched: false, diagnostics: [diagnostic { message: string_concat("unknown constructor ", name) }] }
  }
}

fn merge(left: check, right: check) -> check {
  check { matched: left.matched && right.matched, diagnostics: append(left.diagnostics, right.diagnostics) }
}

fn append(left: List<diagnostic>, right: List<diagnostic>) -> List<diagnostic> {
  match left {
    [] -> right,
    [item, ..rest] -> [item, ..append(rest, right)]
  }
}

fn join(diagnostics: List<diagnostic>) -> String {
  match diagnostics {
    [] -> "ok",
    [diagnostic { message: message }] -> message,
    [diagnostic { message: message }, ..rest] -> string_concat(message, string_concat("; ", join(rest)))
  }
}

fn main() {
  let check = check_constructor_payload("Pair", [PString("id"), PInt(1)]);
  dbg(join(check.diagnostics))
}
