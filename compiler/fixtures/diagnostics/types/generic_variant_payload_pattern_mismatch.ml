type option<'a> = Some('a) | None

fn render(value: option<i64>) -> String {
  match value {
    Some("one") -> "one",
    _ -> "other"
  }
}

fn main() {
  dbg(render(Some(1)))
}
