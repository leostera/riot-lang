fn is_even(n) {
  if n < 1 { true } else { is_odd(n - 1) }
}

fn is_odd(n) {
  if n < 1 { false } else { is_even(n - 1) }
}

fn main() {
  dbg(is_even(4))
}
