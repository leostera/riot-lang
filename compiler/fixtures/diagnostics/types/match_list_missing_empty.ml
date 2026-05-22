fn main() {
  let items = [1, 2];
  dbg(match items {
    [head, ..tail] -> head
  })
}
