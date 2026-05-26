type option_i64 = Some(i64) | None
type event = Pair(i64, String)

fn main() {
  let value = Some(41);
  let answer = match value {
    Some(n) -> n + 1,
    _ -> 0
  };
  dbg(answer);

  let event = Pair(7, "seven");
  let label = match event {
    Pair(_, text) -> text,
    _ -> "missing"
  };
  dbg(label)
}
