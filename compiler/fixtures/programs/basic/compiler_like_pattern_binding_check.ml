type pattern = PWildcard | PBind(String) | PTuple(List<pattern>) | PConstructor(String, List<pattern>)
type binding_result = { names: List<String>, diagnostics: List<String> }

fn collect(pattern: pattern, seen: List<String>) -> binding_result {
  match pattern {
    PWildcard -> binding_result { names: seen, diagnostics: [] },
    PBind(name) -> if contains(name, seen) {
      binding_result { names: seen, diagnostics: [string_concat("duplicate ", name)] }
    } else {
      binding_result { names: [name, ..seen], diagnostics: [] }
    },
    PTuple(items) -> collect_list(items, seen),
    PConstructor(_, payload) -> collect_list(payload, seen)
  }
}

fn collect_list(patterns: List<pattern>, seen: List<String>) -> binding_result {
  match patterns {
    [] -> binding_result { names: seen, diagnostics: [] },
    [pattern, ..rest] -> {
      let current = collect(pattern, seen);
      let nested = collect_list(rest, current.names);
      binding_result { names: nested.names, diagnostics: append(current.diagnostics, nested.diagnostics) }
    }
  }
}

fn contains(name: String, names: List<String>) -> bool {
  match names {
    [] -> false,
    [head, ..tail] -> head == name || contains(name, tail)
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
    [item, ..rest] -> string_concat(item, string_concat("; ", join(rest)))
  }
}

fn main() {
  let pattern = PConstructor("Pair", [PBind("left"), PTuple([PBind("right"), PBind("left")])]);
  dbg(join(collect(pattern, []).diagnostics))
}
