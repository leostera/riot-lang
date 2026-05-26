use Option

fn main() {
  let value = Some(42);
  dbg(Option.is_some(value));
  dbg(Option.is_none(value));
  dbg(Option.unwrap_or(value, 7));
  dbg(Option.unwrap_or(None, 7))
}
