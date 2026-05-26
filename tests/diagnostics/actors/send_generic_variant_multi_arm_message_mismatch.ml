fn main() {
  let worker = spawn {
    receive {
      Some(value) -> value + 1,
      None -> (),
    }
  };
  send(worker, Some("oops"))
}
