fn f(value) {
  value
}

fn main() {
  let x = Point { x: 1 + 2, y: f(3) };
  dbg(x)
}
