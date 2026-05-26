fn is_even(n: i64) -> bool {
  if n < 1 { true } else { is_odd(n - 1) }
}

fn is_odd(n: i64) {
  if n < 1 { false } else { is_even(n - 1) }
}

fn main() {
  dbg(is_even(4))
}
