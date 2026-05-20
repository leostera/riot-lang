fn main() {
  let a = spawn {
    receive { msg -> dbg(msg) };
    receive { msg -> dbg(msg) };
    receive { msg -> dbg(msg) }
  };
  let b = spawn {
    receive { msg -> dbg(msg) };
    receive { msg -> dbg(msg) };
    receive { msg -> dbg(msg) }
  };
  send(a, "a1");
  send(a, "a2");
  send(a, "a3");
  send(b, "b1");
  send(b, "b2");
  send(b, "b3")
}
