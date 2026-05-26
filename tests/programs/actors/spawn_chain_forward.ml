fn main() {
  let parent = spawn {
    let child = spawn {
      receive { msg -> dbg(msg) }
    };
    receive { msg -> send(child, msg) }
  };
  send(parent, "through child")
}
