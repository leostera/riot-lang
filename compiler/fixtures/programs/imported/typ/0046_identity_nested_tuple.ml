fn id(x) {
  x
}

fn main() {
  let value = ((0, 1), true);
  let answer = id(value);
  dbg(answer)
}
