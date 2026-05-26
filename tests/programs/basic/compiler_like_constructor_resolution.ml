type path = { module_name: String, name: String }
type constructor_info = { module_name: String, name: String, arity: i64 }
type diagnostic = { message: String }
type lookup = Found(constructor_info) | Missing

fn same_path(path: path, constructor: constructor_info) -> bool {
  path.module_name == constructor.module_name && path.name == constructor.name
}

fn find_constructor(path: path, constructors: List<constructor_info>) -> lookup {
  match constructors {
    [] -> Missing,
    [candidate, ..rest] -> if same_path(path, candidate) { Found(candidate) } else { find_constructor(path, rest) }
  }
}

fn arity_message(path: path) -> String {
  string_concat(path.module_name, string_concat(".", string_concat(path.name, " arity mismatch")))
}

fn check_pattern(path: path, actual_arity: i64, constructors: List<constructor_info>) -> diagnostic {
  match find_constructor(path, constructors) {
    Found(found) -> if found.arity == actual_arity { diagnostic { message: "ok" } } else { diagnostic { message: arity_message(path) } },
    Missing -> diagnostic { message: string_concat("unknown constructor ", string_concat(path.module_name, string_concat(".", path.name))) }
  }
}

fn main() {
  let constructors = [constructor_info { module_name: "Result", name: "Ok", arity: 1 }, constructor_info { module_name: "Result", name: "Err", arity: 1 }];
  let missing = check_pattern(path { module_name: "Result", name: "Missing" }, 1, constructors);
  let wrong_arity = check_pattern(path { module_name: "Result", name: "Ok" }, 2, constructors);
  dbg(string_concat(missing.message, string_concat(";", wrong_arity.message)))
}
