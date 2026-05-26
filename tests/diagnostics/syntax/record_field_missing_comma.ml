type span = { text: String, line: i64 }

fn main() {
  let value = span { text: "answer" line: 1 };
  println(value.text)
}
