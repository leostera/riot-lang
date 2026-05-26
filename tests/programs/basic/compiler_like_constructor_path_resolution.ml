type path = { segments: List<String> }
type diagnostic = { message: String }

fn join_path(segments: List<String>) -> String {
  match segments {
    [] -> "",
    [segment] -> segment,
    [segment, ..rest] -> string_concat(segment, string_concat(".", join_path(rest)))
  }
}

fn check_constructor_path(path: path) -> diagnostic {
  match path.segments {
    [] -> diagnostic { message: "empty constructor path" },
    [_] -> diagnostic { message: "local constructor" },
    [_, _] -> diagnostic { message: "imported constructor" },
    [_, _, .._] -> diagnostic { message: string_concat("unsupported nested constructor ", join_path(path.segments)) },
    [_, .._] -> diagnostic { message: "unreachable short constructor path" }
  }
}

fn main() {
  let local = check_constructor_path(path { segments: ["Some"] });
  let imported = check_constructor_path(path { segments: ["Result", "Ok"] });
  let nested = check_constructor_path(path { segments: ["Result", "Ok", "extra"] });
  dbg(string_concat(local.message, string_concat(";", string_concat(imported.message, string_concat(";", nested.message)))))
}
