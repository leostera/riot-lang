use Result

fn main() {
  let value = Ok(42);
  let failure = Err("no");
  dbg(Result.is_ok(value));
  dbg(Result.is_err(value));
  dbg(Result.is_ok(failure));
  dbg(Result.is_err(failure));
  dbg(Result.unwrap_or(value, 7));
  dbg(Result.unwrap_or(failure, 7))
}
