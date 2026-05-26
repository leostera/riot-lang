use Result

fn main() {
  let value = Ok(1);
  dbg(match value {
    Result.Missing(x) -> x,
    _ -> 0
  })
}
