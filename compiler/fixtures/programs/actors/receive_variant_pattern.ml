type result = Ok(i64) | Err(String)

fn main() {
  let worker = spawn {
    receive {
      Ok(value) -> dbg(value + 1),
      Err(message) -> dbg(message)
    };
    receive {
      Ok(value) -> dbg(value + 1),
      Err(message) -> dbg(message)
    }
  };
  send(worker, Err("bad"));
  send(worker, Ok(41))
}
