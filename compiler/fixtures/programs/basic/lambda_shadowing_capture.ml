fn main() {
  let a = 1;
  let f = fn(ignored) { a };
  let a = 2;
  dbg(f(()))
}
