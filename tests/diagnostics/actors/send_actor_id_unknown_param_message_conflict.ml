fn send_both(worker: actor_id<_>) {
  send(worker, "go");
  send(worker, 1);
  ()
}

fn main() {
  let worker = spawn {
    receive { "go" -> () };
    receive { 1 -> () }
  };
  send_both(worker)
}
