fn inc(x: i64) -> i64 {
  x + 1
}

fn choose(flag: bool) -> i64 {
  if flag {
    10
  } else {
    20
  }
}

fn main() {
  dbg(inc(choose(true)))
}
