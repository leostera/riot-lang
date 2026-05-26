fn main() {
  let pair = ("token", "parser");
  match pair {
    (left right) -> println(left),
    _ -> println("unreachable")
  }
}
