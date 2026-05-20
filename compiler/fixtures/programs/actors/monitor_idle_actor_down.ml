fn main() {
  let pid = spawn {
    receive { msg -> dbg(msg) }
  };
  monitor(pid)
}
