fn main() {
  let add = fn(n) { fn(x) { x + n } };
  dbg(add(1)(2))
}
