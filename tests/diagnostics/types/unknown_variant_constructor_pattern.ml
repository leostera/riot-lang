type option = Some(i64) | None

fn main() {
  let value = Some(1);
  dbg(match value {
    Missing(x) -> x,
    _ -> 0
  })
}
