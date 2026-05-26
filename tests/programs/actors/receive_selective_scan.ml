fn main() {
  let worker = spawn {
    receive {
      "go" -> dbg("matched")
    };
    receive {
      msg -> dbg(msg)
    }
  };
  send(worker, "skip");
  send(worker, "go")
}
