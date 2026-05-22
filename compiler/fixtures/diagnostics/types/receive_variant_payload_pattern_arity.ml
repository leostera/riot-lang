type msg = Ping(i64)

fn main() {
  let worker = spawn {
    receive {
      Ping(a, b) -> dbg(a)
    }
  };
  send(worker, Ping(1))
}
