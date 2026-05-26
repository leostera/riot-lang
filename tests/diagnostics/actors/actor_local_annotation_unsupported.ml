fn main() {
  let worker = spawn {
    let count: i64 = 1;
    receive {
      _ -> println("done")
    }
  };
  monitor(worker)
}
