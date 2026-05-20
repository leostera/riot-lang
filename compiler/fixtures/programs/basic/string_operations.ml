fn prefix() -> string { "riot" }
fn suffix() -> string { " lang" }

fn main() {
  dbg(string_len(""));
  dbg(string_len(prefix()));
  dbg(string_concat("hello", " world"));
  let joined = string_concat(prefix(), suffix());
  dbg(joined);
  dbg(string_concat(string_concat("a", "b"), "c"))
}
