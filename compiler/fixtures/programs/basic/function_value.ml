fn add(n, x) {
  x + n
}

fn main() {
  let f = add;
  dbg(f(2)(40))
}
