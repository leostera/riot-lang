fn main() {
  let pid = spawn {
    receive { msg -> dbg(msg) };
    receive { msg -> dbg(msg) };
    receive { msg -> dbg(msg) }
  };
  send(pid, "first");
  send(pid, "second");
  send(pid, "third")
}
