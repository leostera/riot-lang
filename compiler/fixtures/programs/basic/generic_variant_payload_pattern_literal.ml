type option<'a> = Some('a) | None

fn render(value: option<i64>) -> String {
  match value {
    Some(1) -> "one",
    Some(_) -> "many",
    None -> "none"
  }
}

fn main() {
  dbg(render(Some(1)))
}
