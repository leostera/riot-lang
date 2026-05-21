fn count(values: List<String>) -> i64 {
  match values {
    [] -> 0,
    [_, ..rest] -> 1 + count(rest),
    _ -> 999
  }
}

fn classify(args: List<String>) -> String {
  match args {
    [] -> "empty",
    ["--help", .._] -> "help",
    ["--output", value, .._] -> value,
    [other, .._] -> other,
    _ -> "unknown"
  }
}

fn main() {
  dbg(count(["a", "b", "c"]));
  dbg(classify([]));
  dbg(classify(["--help"]));
  dbg(classify(["--output", "path.txt"]));
  dbg(classify(["input.ml"]))
}
