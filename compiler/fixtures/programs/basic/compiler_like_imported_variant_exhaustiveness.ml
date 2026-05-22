type constructor = { module: String, type_name: String, name: String }
type pattern = PConstructor(String, String) | PWildcard
type variant_case = { module: String, type_name: String, constructors: List<String> }

fn expected_constructors(type_module: String, type_name: String, variants: List<variant_case>) -> List<String> {
  match variants {
    [] -> [],
    [variant_case { module: module, type_name: candidate, constructors: constructors }, ..rest] -> if module == type_module && candidate == type_name { constructors } else { expected_constructors(type_module, type_name, rest) }
  }
}

fn covered_constructors(patterns: List<pattern>, type_module: String) -> List<String> {
  match patterns {
    [] -> [],
    [PWildcard, .._] -> ["*"],
    [PConstructor(module, name), ..rest] -> if module == type_module { [name, ..covered_constructors(rest, type_module)] } else { covered_constructors(rest, type_module) }
  }
}

fn contains(needle: String, haystack: List<String>) -> bool {
  match haystack {
    [] -> false,
    [item, ..rest] -> if item == needle { true } else { contains(needle, rest) }
  }
}

fn covers_all(expected: List<String>, covered: List<String>) -> bool {
  if contains("*", covered) {
    true
  } else {
    match expected {
      [] -> true,
      [item, ..rest] -> if contains(item, covered) { covers_all(rest, covered) } else { false }
    }
  }
}

fn render(result: bool) -> String {
  if result { "exhaustive" } else { "missing" }
}

fn main() {
  let variants = [variant_case { module: "Options", type_name: "option", constructors: ["Some", "None"] }];
  let patterns = [PConstructor("Options", "Some"), PConstructor("Options", "None")];
  let expected = expected_constructors("Options", "option", variants);
  dbg(render(covers_all(expected, covered_constructors(patterns, "Options"))))
}
