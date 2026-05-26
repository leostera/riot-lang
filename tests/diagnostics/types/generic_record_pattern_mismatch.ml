type box<'a> = { value: 'a }

fn render(item: box<i64>) -> String {
  match item {
    box { value: "one" } -> "one",
    _ -> "other"
  }
}

fn main() {
  dbg(render(box { value: 1 }))
}
