type path = { segments: List<String> }
type expr = LocalValue(String) | LocalCall(String) | PathValue(path) | PathCall(path) | ConstructorPattern(String, i64, i64)
type diagnostic = { message: String }

fn join_path(segments: List<String>) -> String {
  match segments {
    [] -> "",
    [segment] -> segment,
    [segment, ..rest] -> string_concat(segment, string_concat(".", join_path(rest)))
  }
}

fn classify_path(prefix: String, path: path) -> diagnostic {
  match path.segments {
    [] -> diagnostic { message: string_concat(prefix, " empty path") },
    [_] -> diagnostic { message: string_concat(prefix, " local") },
    [_, _] -> diagnostic { message: string_concat(prefix, " imported") },
    [_, _, .._] -> diagnostic { message: string_concat(prefix, string_concat(" unsupported nested ", join_path(path.segments))) },
    [_, .._] -> diagnostic { message: string_concat(prefix, " unreachable") }
  }
}

fn classify_constructor_payload(name: String, expected: i64, actual: i64) -> diagnostic {
  if expected == actual {
    diagnostic { message: string_concat(name, " payload ok") }
  } else {
    diagnostic { message: string_concat(name, " constructor payload arity") }
  }
}

fn classify_expr(expr: expr) -> diagnostic {
  match expr {
    LocalValue(name) -> diagnostic { message: string_concat("value local ", name) },
    LocalCall(name) -> diagnostic { message: string_concat("call local ", name) },
    PathValue(path) -> classify_path("value", path),
    PathCall(path) -> classify_path("call", path),
    ConstructorPattern(name, expected, actual) -> classify_constructor_payload(name, expected, actual)
  }
}

fn append(message: String, tail: String) -> String {
  if tail == "" {
    message
  } else {
    string_concat(message, string_concat(";", tail))
  }
}

fn render(exprs: List<expr>) -> String {
  match exprs {
    [] -> "",
    [expr, ..rest] -> {
      let diag = classify_expr(expr);
      append(diag.message, render(rest))
    }
  }
}

fn main() {
  dbg(render([
    PathValue(path { segments: ["Module", "value", "field"] }),
    PathCall(path { segments: ["Module", "value", "call"] }),
    ConstructorPattern("Some", 1, 2)
  ]))
}
