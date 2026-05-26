fn classify(n) {
  match n {
    0 -> "zero",
    1 -> "one",
    _ -> "many"
  }
}

fn main() {
  dbg(classify(1));
  dbg(match "go" {
    "stop" -> 0,
    "go" -> 1,
    _ -> 2
  });
  dbg(match 41 {
    value -> value + 1
  })
}
