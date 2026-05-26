type box<'a> = { value: 'a }

fn main() {
  let item = box { value: 1 };
  dbg(string_concat(item.value, ""))
}
