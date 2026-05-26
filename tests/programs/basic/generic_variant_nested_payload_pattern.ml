type pair_box<'a, 'b> = Pair(('a, 'b))

fn render(value: pair_box<i64, String>) -> String {
  match value {
    Pair((1, name)) -> name,
    Pair((_, name)) -> string_concat("other:", name)
  }
}

fn main() {
  dbg(render(Pair((1, "one"))))
}
