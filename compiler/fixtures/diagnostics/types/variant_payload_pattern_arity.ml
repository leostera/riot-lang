type option_i64 = Some(i64) | None

fn main() {
  let value = Some(1);
  dbg(match value {
    Some(left, right) -> left,
    _ -> 0
  })
}
