fn main() {
  let pid = spawn {
    let x = 1;
    let x = 2;
    receive { msg -> dbg(msg) }
  };
  send(pid, "ok")
}
