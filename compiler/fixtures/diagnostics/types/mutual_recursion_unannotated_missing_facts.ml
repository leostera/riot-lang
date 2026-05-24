fn left(value) {
  right(value)
}

fn right(value) {
  left(value)
}

fn main() {
  dbg("unreachable")
}
