fn main() {
  let worker = spawn {
    receive {
      () -> dbg("unit")
    };
    receive {
      msg -> dbg(msg)
    }
  };
  send(worker, "skip");
  send(worker, ())
}
