type option_i64 = Some(i64) | None

fn render(value: option_i64) -> String {
  match value {
    Some(1) -> "one",
    Some(2) -> "two",
    Some(_) -> "many",
    None -> "none"
  }
}

fn main() {
  let one = Some(1);
  let two = Some(2);
  let none = None;
  dbg(string_concat(render(one), string_concat(",", string_concat(render(two), string_concat(",", render(none))))))
}
