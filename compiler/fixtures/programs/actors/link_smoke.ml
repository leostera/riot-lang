fn main() {
  let pid = spawn {
    receive { msg -> dbg(msg) }
  };
  link(pid);
  send(pid, "linked")
}
