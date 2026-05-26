type monitor_down = Down(actor_id<_>)

fn make_sender() {
  spawn { receive { "go" -> dbg("tuple target sent") } }
}

fn make_idle() {
  spawn { receive { () -> () } }
}

fn main() {
  let senders = (make_sender(), make_sender());
  send(senders.0, "go");
  spawn {
    let watchers = (make_idle(), make_idle());
    monitor(watchers.1);
    dbg("tuple target monitored");
    receive { Down(_) -> () }
  };
  spawn {
    let links = (make_idle(), make_idle());
    link(links.0);
    dbg("tuple target linked");
    receive { () -> () }
  };
  ()
}
