use Result

fn main() {
  let value = Ok(1);
  dbg(Result.is_ok.extra(value))
}
