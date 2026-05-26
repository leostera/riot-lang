type box<'a> = { value: 'a }

fn main() {
  let item = box { value: 1 };
  let label = match item {
    box { value: value } -> string_concat(value, "")
  };
  dbg(label)
}
