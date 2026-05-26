fn main() {
  let root = spawn {
    let middle = spawn {
      let leaf = spawn {
        receive { msg -> dbg(msg) }
      };
      receive { msg -> send(leaf, msg) }
    };
    receive { msg -> send(middle, msg) }
  };
  send(root, "through two hops")
}
