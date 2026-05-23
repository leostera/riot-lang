fn make_sender_factory() {
  fn(_) {
    spawn { receive { "go" -> () } }
  }
}

fn main() {
  let factory = make_sender_factory();
  let worker = factory(());
  send(worker, 1)
}
