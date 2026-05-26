fn fact(n) {
  if n < 2 {
    1
  } else {
    n * fact(n - 1)
  }
}

fn main() {
  dbg(fact(5))
}
