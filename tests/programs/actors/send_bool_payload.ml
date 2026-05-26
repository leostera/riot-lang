fn main() {
  let out = spawn {
    receive { msg -> dbg(msg) }
  };
  send(out, true)
}
