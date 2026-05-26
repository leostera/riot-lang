fn main() {
  let worker = spawn {
    receive {
      (1, value) -> dbg(value)
    }
  };
  send(worker, (1, 2))
}
