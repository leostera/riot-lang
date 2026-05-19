fn keep(x, ignored) {
  x
}

fn main() {
  let value = ("x", 'y');
  let answer = keep(value, ());
  dbg(answer)
}
