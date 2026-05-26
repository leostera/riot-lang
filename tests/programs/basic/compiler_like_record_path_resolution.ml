type path = { segments: List<String> }
type diagnostic = { message: String }

fn join_path(segments: List<String>) -> String {
  match segments {
    [] -> "",
    [segment] -> segment,
    [segment, ..rest] -> string_concat(segment, string_concat(".", join_path(rest)))
  }
}

fn check_record_path(path: path) -> diagnostic {
  match path.segments {
    [] -> diagnostic { message: "empty record path" },
    [_] -> diagnostic { message: "local record" },
    [_, _] -> diagnostic { message: "imported record" },
    [_, _, .._] -> diagnostic { message: string_concat("unsupported nested record ", join_path(path.segments)) },
    [_, .._] -> diagnostic { message: "unreachable short record path" }
  }
}

fn main() {
  let local = check_record_path(path { segments: ["box"] });
  let imported = check_record_path(path { segments: ["Boxes", "box"] });
  let nested = check_record_path(path { segments: ["Boxes", "box", "extra"] });
  dbg(string_concat(local.message, string_concat(";", string_concat(imported.message, string_concat(";", nested.message)))))
}
