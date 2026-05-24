fn left(n) {
  if n < 1 { 0 } else { right(n - 1) }
}

fn right(n) {
  if n < 1 { "done" } else { left(n - 1) }
}

fn main() {
  dbg(left(2))
}
