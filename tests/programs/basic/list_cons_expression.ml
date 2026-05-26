fn length(values: List<i64>) -> i64 {
  match values {
    [] -> 0,
    [_, ..rest] -> 1 + length(rest)
  }
}

fn main() {
  let tail = [2, 3];
  let values = [0, 1, ..tail];
  dbg(length(values));
  dbg(list_get(values, 0));
  dbg(list_get(values, 3))
}
