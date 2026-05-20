type option_i64 = Some(i64) | None
type event = Pair(i64, string)

fn wrap(value: i64) {
  Some(value)
}

fn pair() {
  Pair(1, "one")
}

fn main() {
  dbg(wrap(41));
  dbg(pair());
  dbg(None)
}
