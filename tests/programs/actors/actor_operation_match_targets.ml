type monitor_down = Down(actor_id<_>)

type choice = Sender | Idle

fn make_sender() {
  spawn { receive { "go" -> dbg("match target sent") } }
}

fn make_idle() {
  spawn { receive { () -> () } }
}

fn main() {
  let sender = match Sender {
    Sender -> make_sender(),
    Idle -> make_sender()
  };
  send(sender, "go");
  spawn {
    let monitored = match Idle {
      Sender -> make_idle(),
      Idle -> make_idle()
    };
    monitor(monitored);
    dbg("match target monitored");
    receive { Down(_) -> () }
  };
  spawn {
    let linked = match Sender {
      Sender -> make_idle(),
      Idle -> make_idle()
    };
    link(linked);
    dbg("match target linked");
    receive { () -> () }
  };
  ()
}
