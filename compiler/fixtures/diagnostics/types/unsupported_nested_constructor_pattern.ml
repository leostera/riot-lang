use Result

fn main() {
  let value = Ok(1);
  dbg(match value {
    Result.Ok.extra(x) -> x,
    _ -> 0
  })
}
