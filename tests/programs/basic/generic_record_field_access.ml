type box<'a> = { value: 'a }

fn main() {
  let item = box { value: 41 };
  dbg(item.value + 1)
}
