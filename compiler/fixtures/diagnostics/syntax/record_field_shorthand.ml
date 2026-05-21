type symbol = { name: String }

fn main() {
  let name = "input";
  let value = symbol { name };
  println(value.name)
}
