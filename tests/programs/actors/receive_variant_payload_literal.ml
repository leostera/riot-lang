type result = Ok(i64) | Err(String)

fn main() {
  let worker = spawn {
    receive {
      Ok(41) -> dbg("forty-one"),
      Ok(value) -> dbg(value),
      Err(message) -> dbg(message)
    }
  };
  send(worker, Ok(41))
}
