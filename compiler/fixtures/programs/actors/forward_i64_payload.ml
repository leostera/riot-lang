fn main() {
  let out = spawn {
    receive { msg -> dbg(msg) }
  };
  let relay = spawn {
    receive { msg -> send(out, msg) }
  };
  send(relay, 7)
}
