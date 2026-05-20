fn main() {
  let pid = spawn {
    receive { msg -> dbg(msg) };
    receive { msg -> dbg(msg) };
    receive { msg -> dbg(msg) };
    receive { msg -> dbg(msg) };
    receive { msg -> dbg(msg) }
  };
  send(pid, "one");
  send(pid, "two");
  send(pid, "three");
  send(pid, "four");
  send(pid, "five")
}
