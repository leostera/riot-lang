fn duplicate(x) {
  (x, x)
}

fn main() {
  let answer = duplicate((0, 'x'));
  dbg(answer)
}
