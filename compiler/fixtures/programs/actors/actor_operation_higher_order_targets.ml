fn make_sender_factory(prefix: String) {
  fn(label) {
    spawn { receive { "go" -> dbg(string_concat(prefix, label)) } }
  }
}

fn make_idle_closure() {
  let idle = spawn { receive { () -> () } };
  fn(_) { idle }
}

fn main() {
  let factory = make_sender_factory("higher ");
  let worker = factory("order");
  send(worker, "go");
  let idle_factory = make_idle_closure();
  monitor(idle_factory(()));
  link(idle_factory(()));
  dbg("higher order watched");
  ()
}
