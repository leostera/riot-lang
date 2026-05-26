fn main() {
  let items = [1, 2];
  dbg(match items {
    [value, value] -> value,
    _ -> 0
  })
}
