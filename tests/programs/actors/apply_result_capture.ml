fn main() {
  let inc = fn(x) { x + 1 };
  let worker = spawn {
    receive {
      () -> {
        let y = inc(41);
        let child = spawn {
          receive {
            () -> dbg(y)
          }
        };
        send(child, ())
      }
    }
  };
  send(worker, ())
}
