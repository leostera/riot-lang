type box<'a> = { value: 'a }

fn render(item: box<i64>) -> String {
  match item {
    box { value: 1 } -> "one",
    box { value: _ } -> "many"
  }
}

fn main() {
  dbg(render(box { value: 1 }))
}
