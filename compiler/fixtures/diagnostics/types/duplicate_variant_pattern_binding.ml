type pair = Pair(i64, i64)

fn main() {
  let pair = Pair(1, 2);
  dbg(match pair {
    Pair(value, value) -> value,
    _ -> 0
  })
}
