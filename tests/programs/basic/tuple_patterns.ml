fn pair() {
  (7, "seven")
}

fn main() {
  let label = match pair() {
    (_, text) -> text,
    _ -> "missing"
  };
  dbg(label);

  let number = match (1, 2) {
    (left, right) -> left + right,
    _ -> 0
  };
  dbg(number)
}
