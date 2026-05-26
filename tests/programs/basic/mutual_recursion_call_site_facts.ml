fn left(value) {
  right(value)
}

fn right(value) {
  left(value)
}

fn helper() {
  left(1) + 1
}

fn main() {
  dbg("ok")
}
