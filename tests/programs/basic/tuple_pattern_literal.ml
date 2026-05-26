fn main() {
  let value = (1, 2);
  dbg(match value {
    (1, right) -> right,
    _ -> 0
  })
}
