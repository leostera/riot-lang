type box<'a> = { value: 'a }

fn main() {
  let item = box { value: 1 };
  let label = match item {
    box { value: value } -> if value == 1 { "one" } else { "other" }
  };
  dbg(label)
}
