type monitor_down = Down(actor_id<_>)

fn make_string_worker() {
  spawn {
    receive { "go" -> dbg("local factory sent") }
  }
}

fn make_idle_worker() {
  spawn {
    receive { () -> () }
  }
}

fn main() {
  let sender = make_string_worker();
  send(sender, "go");
  spawn {
    let monitored = make_idle_worker();
    monitor(monitored);
    dbg("local factory monitored");
    receive { Down(_) -> () }
  };
  spawn {
    let linked = make_idle_worker();
    link(linked);
    dbg("local factory linked");
    receive { () -> () }
  };
  ()
}
