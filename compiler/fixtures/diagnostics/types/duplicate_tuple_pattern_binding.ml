fn main() {
  let pair = (1, 2);
  dbg(match pair {
    (value, value) -> value,
    _ -> 0
  })
}
