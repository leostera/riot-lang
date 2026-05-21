fn main() {
  let values = ["head", "tail"];
  match values {
    [head tail] -> println(head),
    _ -> println("unreachable")
  }
}
