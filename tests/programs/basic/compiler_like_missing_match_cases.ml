type case = EmptyList | NonEmptyList | Constructor(String)
type pattern = PEmptyList | PConsList | PConstructor(String) | PWildcard

fn case_name(case: case) -> String {
  match case {
    EmptyList -> "[]",
    NonEmptyList -> "[head, ..tail]",
    Constructor(name) -> name
  }
}

fn covers(pattern: pattern, case: case) -> bool {
  match pattern {
    PWildcard -> true,
    PEmptyList -> match case { EmptyList -> true, _ -> false },
    PConsList -> match case { NonEmptyList -> true, _ -> false },
    PConstructor(name) -> match case { Constructor(case_name) -> name == case_name, _ -> false }
  }
}

fn is_covered(case: case, patterns: List<pattern>) -> bool {
  match patterns {
    [] -> false,
    [pattern, ..rest] -> if covers(pattern, case) { true } else { is_covered(case, rest) }
  }
}

fn missing_cases(expected: List<case>, patterns: List<pattern>) -> List<String> {
  match expected {
    [] -> [],
    [case, ..rest] -> if is_covered(case, patterns) { missing_cases(rest, patterns) } else { [case_name(case), ..missing_cases(rest, patterns)] }
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
  let list_missing = missing_cases([EmptyList, NonEmptyList], [PConsList]);
  let variant_missing = missing_cases([Constructor("Some"), Constructor("None")], [PConstructor("Some")]);
  dbg(string_concat(join(list_missing), string_concat(";", join(variant_missing))))
}
