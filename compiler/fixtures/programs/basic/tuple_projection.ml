fn pair() -> (string, i64) { ("riot", 42) }
fn nested() -> ((i64, i64), string) { ((10, 20), "done") }

fn main() {
  dbg(("literal", 1).0);
  let local = ("local", 2);
  dbg(local.1);
  dbg(pair().0);
  dbg((nested().0).1)
}
