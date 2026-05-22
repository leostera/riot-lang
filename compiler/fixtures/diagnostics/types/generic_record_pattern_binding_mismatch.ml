type box<'a> = { value: 'a }

fn render(item: box<i64>) -> String {
  match item {
    box { value: value } -> string_concat(value, "")
  }
}

fn main() {
  dbg(render(box { value: 1 }))
}
