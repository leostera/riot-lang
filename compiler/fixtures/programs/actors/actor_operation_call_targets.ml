type monitor_down = Down(actor_id<_>)

fn make_string_worker() {
  spawn {
    receive { "go" -> dbg("factory sent") }
  }
}

fn make_idle_worker() {
  spawn {
    receive { () -> () }
  }
}

fn main() {
  send(make_string_worker(), "go");
  spawn {
    monitor(make_idle_worker());
    dbg("factory monitored");
    receive { Down(_) -> () }
  };
  spawn {
    link(make_idle_worker());
    dbg("factory linked");
    receive { () -> () }
  };
  ()
}
