fn main() {
  let value = (1, 2);
  dbg(match value {
    (left, middle, right) -> left,
    _ -> 0
  })
}
