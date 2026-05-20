fn main() {
  let pid = spawn {
    receive { msg -> dbg(msg) }
  };
  send(pid, (1, true))
}
