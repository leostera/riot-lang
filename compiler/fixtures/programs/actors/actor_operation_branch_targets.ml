type monitor_down = Down(actor_id<_>)

fn main() {
  let sender = if true {
    spawn { receive { "go" -> dbg("branch send left") } }
  } else {
    spawn { receive { "go" -> dbg("branch send right") } }
  };
  send(sender, "go");
  spawn {
    let monitored = if true {
      spawn { receive { () -> () } }
    } else {
      spawn { receive { () -> () } }
    };
    monitor(monitored);
    dbg("branch monitored");
    receive { Down(_) -> () }
  };
  spawn {
    let linked = if true {
      spawn { receive { () -> () } }
    } else {
      spawn { receive { () -> () } }
    };
    link(linked);
    dbg("branch linked");
    receive { () -> () }
  };
  ()
}
