type mixed_box = { worker: actor_id<_> }

fn make_mixed(label: String) {
  spawn {
    receive { "go" -> dbg(string_concat(label, " string")) };
    receive { 1 -> dbg(string_concat(label, " i64")) }
  }
}

fn main() {
  let workers = mixed_box { worker: make_mixed("record unknown") };
  send(workers.worker, "go");
  send(workers.worker, 1);
  ()
}
