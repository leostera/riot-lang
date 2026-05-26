fn main() {
  let worker = spawn {
    receive { }
  };
  send(worker, "hello")
}
