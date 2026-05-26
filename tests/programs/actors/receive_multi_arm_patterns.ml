fn main() {
  let worker = spawn {
    receive {
      "ping" -> dbg("pong"),
      _ -> dbg("miss")
    };
    receive {
      41 -> dbg(42),
      _ -> dbg(0)
    };
    receive {
      true -> dbg("true"),
      _ -> dbg("other")
    }
  };
  send(worker, "ping");
  send(worker, 41);
  send(worker, false)
}
