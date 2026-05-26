fn main() {
  let worker = spawn {
    receive {
      (left, right) -> dbg(left + right)
    }
  };
  send(worker, (19, 23))
}
