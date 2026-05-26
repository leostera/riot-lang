type monitor_down = Down(actor_id<_>)

type sender_box = { sender: actor_id<String>, backup: actor_id<String> }
type idle_box = { primary: actor_id<()>, secondary: actor_id<()> }

fn make_sender() {
  spawn { receive { "go" -> dbg("record target sent") } }
}

fn make_idle() {
  spawn { receive { () -> () } }
}

fn main() {
  let senders = sender_box { sender: make_sender(), backup: make_sender() };
  send(senders.sender, "go");
  spawn {
    let watchers = idle_box { primary: make_idle(), secondary: make_idle() };
    monitor(watchers.secondary);
    dbg("record target monitored");
    receive { Down(_) -> () }
  };
  spawn {
    let links = idle_box { primary: make_idle(), secondary: make_idle() };
    link(links.primary);
    dbg("record target linked");
    receive { () -> () }
  };
  ()
}
