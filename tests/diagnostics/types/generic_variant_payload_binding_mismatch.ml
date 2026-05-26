type option<'a> = Some('a) | None

fn render(item: option<i64>) -> String {
  match item {
    Some(value) -> string_concat(value, ""),
    None -> "none"
  }
}

fn main() {
  dbg(render(Some(1)))
}
