fn main() {
  let self_apply = fn(value) { value(value) };
  println("unreachable")
}
