external trace : 'a -> unit = "riot_rt_dbg_value"

type result = Ok(i64) | Err(string)

fn main() {
  trace((1, true));
  trace(Ok(42));
  dbg("done")
}
