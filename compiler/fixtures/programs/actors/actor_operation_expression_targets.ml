type monitor_down = Down(actor_id<_>)

fn main() {
  send(spawn { receive { "go" -> dbg("sent") } }, "go");
  spawn {
    monitor(spawn { receive { msg -> dbg(msg) } });
    receive { Down(_) -> dbg("down") }
  };
  spawn {
    link(spawn { receive { msg -> dbg(msg) } });
    dbg("linked");
    receive { () -> () }
  };
  ()
}
