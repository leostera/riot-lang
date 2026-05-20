fn main() {
  let pid = spawn {
    dbg("not a receive loop")
  };
  send(pid, "hello")
}
