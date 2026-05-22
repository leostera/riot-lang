type constructor = { name: String }
type pattern = PWildcard | PConstructor(String) | PListEmpty | PListCons
type typ = TVariant(List<constructor>) | TList | TInt

type coverage = { exhaustive: bool, missing: List<String> }

fn covers_constructor(name: String, patterns: List<pattern>) -> bool {
  match patterns {
    [] -> false,
    [PWildcard, .._] -> true,
    [PConstructor(actual), ..rest] -> actual == name || covers_constructor(name, rest),
    [_, ..rest] -> covers_constructor(name, rest)
  }
}

fn variant_coverage(constructors: List<constructor>, patterns: List<pattern>) -> coverage {
  match constructors {
    [] -> coverage { exhaustive: true, missing: [] },
    [constructor { name: name }, ..rest] -> {
      let nested = variant_coverage(rest, patterns);
      if covers_constructor(name, patterns) {
        nested
      } else {
        coverage { exhaustive: false, missing: [name, ..nested.missing] }
      }
    }
  }
}

fn has_empty(patterns: List<pattern>) -> bool {
  match patterns {
    [] -> false,
    [PWildcard, .._] -> true,
    [PListEmpty, .._] -> true,
    [_, ..rest] -> has_empty(rest)
  }
}

fn has_cons(patterns: List<pattern>) -> bool {
  match patterns {
    [] -> false,
    [PWildcard, .._] -> true,
    [PListCons, .._] -> true,
    [_, ..rest] -> has_cons(rest)
  }
}

fn list_coverage(patterns: List<pattern>) -> coverage {
  let missing_empty = if has_empty(patterns) { [] } else { ["[]"] };
  let missing_cons = if has_cons(patterns) { [] } else { ["[_]", ..missing_empty] };
  coverage { exhaustive: has_empty(patterns) && has_cons(patterns), missing: missing_cons }
}

fn check(typ: typ, patterns: List<pattern>) -> coverage {
  match typ {
    TVariant(constructors) -> variant_coverage(constructors, patterns),
    TList -> list_coverage(patterns),
    TInt -> coverage { exhaustive: false, missing: ["_"] }
  }
}

fn append(left: List<String>, right: List<String>) -> List<String> {
  match left {
    [] -> right,
    [item, ..rest] -> [item, ..append(rest, right)]
  }
}

fn join(items: List<String>) -> String {
  match items {
    [] -> "ok",
    [item] -> item,
    [item, ..rest] -> string_concat(item, string_concat(",", join(rest)))
  }
}

fn main() {
  let variant = check(TVariant([constructor { name: "Some" }, constructor { name: "None" }]), [PConstructor("Some")]);
  let list = check(TList, [PListEmpty]);
  dbg(string_concat(join(variant.missing), string_concat(";", join(list.missing))))
}
