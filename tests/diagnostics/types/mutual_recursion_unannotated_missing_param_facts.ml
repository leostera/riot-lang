fn left(value) {
  if true { 1 } else { right(value) }
}

fn right(value) {
  if true { 2 } else { left(value) }
}

fn main() {
  dbg("ok")
}
