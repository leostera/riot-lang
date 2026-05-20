fn main() {
  let pid = spawn {
    receive { msg -> dbg(msg) }
  };
  monitor(pid);
  monitor(pid);
  send(pid, "watched twice")
}
