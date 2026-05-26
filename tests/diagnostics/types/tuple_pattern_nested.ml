fn main() {
  let value = (1, 2);
  dbg(match value {
    ("one", right) -> right,
    _ -> 0
  })
}
