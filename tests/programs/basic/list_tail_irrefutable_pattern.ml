type int_list = Ints(List<i64>)

fn sum(items: List<i64>) -> i64 {
  match items {
    [] -> 0,
    [head, ..tail] -> head + sum(tail)
  }
}

fn main() {
  let value = Ints([1, 2, 3]);
  dbg(match value {
    Ints([..items]) -> sum(items)
  })
}
